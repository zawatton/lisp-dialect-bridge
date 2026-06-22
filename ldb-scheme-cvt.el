;;; ldb-scheme-cvt.el --- Scheme (R7RS) core syntax -> Elisp IR -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5: translate a Scheme /core syntax/ subset to Elisp on the shared
;; IR.  Reader is the Phase-1 `ldb-scheme.el' (preprocess + Emacs `read');
;; this module adds the Scheme->IR translator.  Design: docs/design/
;; 03-scheme-core.org (the Lisp-1 -> Lisp-2 decision is locked there).
;;
;; The central problem is Lisp-1 vs Lisp-2 (see L-SCM1): Scheme has one
;; namespace, Elisp two.  We thread a lexical environment `ldb-scheme--env'
;; (locally-bound symbols) and pre-classify top-level defines into
;; functions vs variables (`ldb-scheme--globals').  Then:
;;   * call position (OP a...): OP local -> (funcall OP a...);
;;     else -> (OP' a...) with OP' remapped.
;;   * value position symbol: local -> sym; a known function -> #'sym';
;;     global var -> sym; unknown -> sym.
;; Booleans: #t->t #f->nil (reader); the ()-as-true gap is documented, not
;; rejected.  Continuations / hygienic macros / multiple values / vectors /
;; full numeric tower are rejected loudly.

;;; Code:

(require 'ldb-scheme)
(require 'ldb-ir)
(require 'ldb-emit-elisp)

(define-error 'ldb-scheme-unsupported-form-error
  "Scheme form unsupported by core translator")

(defvar ldb-scheme--env nil
  "Dynamic list of locally-bound symbols in scope (lambda/let/etc. vars).
A symbol in this list, used in call position, becomes a `funcall'.")

(defvar ldb-scheme--globals '(nil)
  "Dynamic (FUNCS . VARS): top-level define'd function and variable names.
Set once per translate by `ldb-scheme--collect-globals'.")

;;;; --- tables ---------------------------------------------------------------

(defconst ldb-scheme--remap
  '((null? . null) (pair? . consp) (list? . listp)
    (eq? . eq) (eqv? . eql) (equal? . equal)
    (zero? . zerop) (positive? . cl-plusp) (negative? . cl-minusp)
    (odd? . cl-oddp) (even? . cl-evenp)
    (number? . numberp) (integer? . integerp) (real? . floatp)
    (string? . stringp) (symbol? . symbolp) (boolean? . booleanp)
    (procedure? . functionp) (char? . characterp) (vector? . vectorp)
    (set-car! . setcar) (set-cdr! . setcdr)
    (display . princ) (write . prin1) (newline . terpri)
    (string-append . concat) (string-length . length)
    (string->symbol . intern) (symbol->string . symbol-name)
    (number->string . number-to-string) (string->number . string-to-number)
    (modulo . mod) (remainder . %) (quotient . /)
    (add1 . 1+) (sub1 . 1-)
    (for-each . cl-mapc) (map . cl-mapcar) (filter . seq-filter)
    (begin . progn)
    (assv . assq) (memv . memql))
  "Scheme operator -> Elisp/cl-lib symbol (head and value position).")

(defun ldb-scheme--remap-head (sym)
  "Map a Scheme operator SYM to its Elisp/cl-lib equivalent (or itself)."
  (or (cdr (assq sym ldb-scheme--remap)) sym))

(defconst ldb-scheme--builtin-functions
  '(car cdr caar cadr caddr cadddr cdar cddr cdddr cons list append reverse
    length not + - * / < > <= >= = 1+ 1- abs min max expt sqrt gcd lcm
    floor ceiling round truncate apply nth nthcdr last
    null consp listp eq eql equal zerop cl-plusp cl-minusp cl-oddp cl-evenp
    numberp integerp floatp stringp symbolp booleanp functionp characterp vectorp
    setcar setcdr princ prin1 terpri concat intern symbol-name
    number-to-string string-to-number mod % cl-mapc cl-mapcar seq-filter
    assoc assq member memq memql)
  "Symbols treated as functions in value position (emitted via a function quote).")

(defun ldb-scheme--function-symbol-p (sym)
  "Non-nil if SYM denotes a function (builtin or remapped) in value position."
  (or (assq sym ldb-scheme--remap)
      (memq sym ldb-scheme--builtin-functions)))

(defconst ldb-scheme--reject-heads
  '(define-syntax let-syntax letrec-syntax syntax-rules syntax-case
    call/cc call-with-current-continuation dynamic-wind
    values call-with-values
    delay delay-force force make-promise
    do case
    parameterize guard with-exception-handler raise raise-continuable
    define-record-type define-values
    quasiquote unquote unquote-splicing)
  "Scheme forms rejected loudly by the core translator (never silent).
Continuations, hygienic macros, multiple values, promises, records and
quasiquote land in later increments.")

;;;; --- pre-pass: classify global defines ------------------------------------

(defun ldb-scheme--collect-globals (forms)
  "Return (FUNCS . VARS): top-level `define'd function and variable names."
  (let (funcs vars)
    (dolist (f forms)
      (when (and (consp f) (eq (car f) 'define))
        (let ((target (cadr f)))
          (cond
           ((consp target) (push (car target) funcs))
           ((and (consp (caddr f)) (eq (car (caddr f)) 'lambda))
            (push target funcs))
           (t (push target vars))))))
    (cons funcs vars)))

;;;; --- lambda lists ---------------------------------------------------------

(defun ldb-scheme--proper-part (l)
  "Return the proper (cons-chain) prefix of a possibly-dotted list L."
  (let (acc)
    (while (consp l) (push (car l) acc) (setq l (cdr l)))
    (nreverse acc)))

(defun ldb-scheme--dotted-tail (l)
  "Return the final non-nil cdr of a dotted list L, or nil if proper."
  (while (consp l) (setq l (cdr l)))
  l)

(defun ldb-scheme--params (params)
  "Translate a Scheme PARAMS spec to an Elisp lambda list.
Handles fixed `(a b)', dotted `(a . rest)' and bare `args' (all-args)."
  (cond
   ((symbolp params) (if params (list '&rest params) nil))
   ((ldb-scheme--dotted-tail params)
    (append (ldb-scheme--proper-part params)
            (list '&rest (ldb-scheme--dotted-tail params))))
   (t params)))

(defun ldb-scheme--param-names (params)
  "Return the list of symbols bound by a Scheme PARAMS spec."
  (cond
   ((symbolp params) (if params (list params) nil))
   ((ldb-scheme--dotted-tail params)
    (append (ldb-scheme--proper-part params)
            (list (ldb-scheme--dotted-tail params))))
   (t params)))

;;;; --- Scheme S-expression -> IR --------------------------------------------

(defun ldb-scheme-form-to-ir (form)
  "Translate a Scheme S-expression FORM into a dialect-neutral IR node."
  (cond
   ((stringp form)  (ldb-ir-make 'literal :form (list :value form :type 'string)  :origin 'scheme))
   ((integerp form) (ldb-ir-make 'literal :form (list :value form :type 'integer) :origin 'scheme))
   ((floatp form)   (ldb-ir-make 'literal :form (list :value form :type 'float)   :origin 'scheme))
   ((keywordp form) (ldb-ir-make 'literal :form (list :value form :type 'keyword) :origin 'scheme))
   ((memq form '(t nil)) (ldb-ir-make 'literal :form (list :value form :type 'bool) :origin 'scheme))
   ((symbolp form)  (ldb-scheme--symbol-to-ir form))
   ((vectorp form)
    (signal 'ldb-scheme-unsupported-form-error (list "vectors unsupported in core v1" form)))
   ((consp form)    (ldb-scheme--compound-to-ir form))
   (t (ldb-ir-make 'literal :form (list :value form :type 'other) :origin 'scheme))))

(defun ldb-scheme--map-forms (forms)
  "Translate FORMS to IR nodes."
  (mapcar #'ldb-scheme-form-to-ir forms))

(defun ldb-scheme--symbol-to-ir (sym)
  "Translate SYM in VALUE position (Lisp-1 -> Lisp-2 resolution)."
  (cond
   ((memq sym ldb-scheme--env)
    (ldb-ir-make 'ref :form (list :name sym) :origin 'scheme))
   ((memq sym (car ldb-scheme--globals))
    (ldb-scheme--function-ref-ir sym))
   ((memq sym (cdr ldb-scheme--globals))
    (ldb-ir-make 'ref :form (list :name sym) :origin 'scheme))
   ((ldb-scheme--function-symbol-p sym)
    (ldb-scheme--function-ref-ir sym))
   (t (ldb-ir-make 'ref :form (list :name sym) :origin 'scheme))))

(defun ldb-scheme--function-ref-ir (sym)
  "Emit `#'SYM'' (the function cell as a value) for a function-valued SYM."
  (ldb-ir-make 'call
               :form (list :fn 'function
                           :args (list (ldb-ir-make 'ref
                                                    :form (list :name (ldb-scheme--remap-head sym))
                                                    :origin 'scheme)))
               :origin 'scheme))

(defun ldb-scheme--compound-to-ir (form)
  "Translate a non-empty list FORM by dispatching on its head."
  (let ((head (car form)))
    (cond
     ((not (symbolp head)) (ldb-scheme--apply-expr-to-ir form))
     ((memq head (list ldb-ir-backquote-symbol ldb-ir-unquote-symbol
                       ldb-ir-splice-symbol))
      (signal 'ldb-scheme-unsupported-form-error
              (list "quasiquote/unquote unsupported in core v1" form)))
     ((memq head ldb-scheme--reject-heads)
      (signal 'ldb-scheme-unsupported-form-error
              (list (format "%S unsupported by Scheme core translator" head) form)))
     ((eq head 'quote) (ldb-ir-make 'quote :form (list :datum (cadr form)) :origin 'scheme))
     ((eq head 'define) (ldb-scheme--define-to-ir form))
     ((eq head 'lambda) (ldb-scheme--lambda-to-ir form))
     ((eq head 'if) (ldb-scheme--if-to-ir form))
     ((eq head 'cond) (ldb-scheme--cond-to-ir form))
     ((eq head 'set!) (ldb-scheme--set!-to-ir form))
     ((eq head 'let)
      (if (and (cadr form) (symbolp (cadr form)))
          (ldb-scheme--named-let-to-ir form)
        (ldb-scheme--plain-let-to-ir form)))
     ((memq head '(let* letrec letrec*)) (ldb-scheme--plain-let-to-ir form))
     (t (ldb-scheme--call-to-ir form)))))

(defun ldb-scheme--call-to-ir (form)
  "Translate a call FORM, inserting `funcall' when the head is a local var."
  (let ((head (car form))
        (args (ldb-scheme--map-forms (cdr form))))
    (if (memq head ldb-scheme--env)
        (ldb-ir-make 'call
                     :form (list :fn 'funcall
                                 :args (cons (ldb-ir-make 'ref :form (list :name head) :origin 'scheme)
                                             args))
                     :origin 'scheme)
      (ldb-ir-make 'call
                   :form (list :fn (ldb-scheme--remap-head head) :args args)
                   :origin 'scheme))))

(defun ldb-scheme--apply-expr-to-ir (form)
  "Translate `((EXPR) ARGS...)' (an applied non-symbol head) via `funcall'."
  (ldb-ir-make 'call
               :form (list :fn 'funcall
                           :args (cons (ldb-scheme-form-to-ir (car form))
                                       (ldb-scheme--map-forms (cdr form))))
               :origin 'scheme))

(defun ldb-scheme--if-to-ir (form)
  "Translate a Scheme `if' (2- or 3-armed)."
  (ldb-ir-make 'if
               :form (list :cond (ldb-scheme-form-to-ir (nth 1 form))
                           :then (ldb-scheme-form-to-ir (nth 2 form))
                           :else (when (> (length form) 3)
                                   (ldb-scheme-form-to-ir (nth 3 form))))
               :origin 'scheme))

(defun ldb-scheme--cond-to-ir (form)
  "Translate a Scheme `cond' (`else' -> t; `=>' clauses rejected)."
  (ldb-ir-make 'cond
               :form (list :clauses
                           (mapcar
                            (lambda (clause)
                              (unless (consp clause)
                                (signal 'ldb-scheme-unsupported-form-error
                                        (list "cond clause must be a list" clause)))
                              (when (and (cdr clause) (eq (cadr clause) '=>))
                                (signal 'ldb-scheme-unsupported-form-error
                                        (list "cond => clause unsupported in core v1" clause)))
                              (let ((test (car clause)))
                                (list (if (eq test 'else)
                                          (ldb-ir-make 'literal :form (list :value t :type 'bool) :origin 'scheme)
                                        (ldb-scheme-form-to-ir test))
                                      (ldb-scheme--map-forms (cdr clause)))))
                            (cdr form)))
               :origin 'scheme))

(defun ldb-scheme--set!-to-ir (form)
  "Translate `(set! VAR EXPR)' to a `setq'."
  (ldb-ir-make 'call
               :form (list :fn 'setq
                           :args (list (ldb-ir-make 'ref :form (list :name (nth 1 form)) :origin 'scheme)
                                       (ldb-scheme-form-to-ir (nth 2 form))))
               :origin 'scheme))

(defun ldb-scheme--lambda-to-ir (form)
  "Translate a Scheme `lambda'; params enter the env for the body."
  (let* ((params (cadr form))
         (names (ldb-scheme--param-names params)))
    (ldb-ir-make 'lambda
                 :form (list :params (ldb-scheme--params params)
                             :cl nil
                             :body (let ((ldb-scheme--env (append names ldb-scheme--env)))
                                     (ldb-scheme--map-forms (cddr form))))
                 :origin 'scheme)))

(defun ldb-scheme--define-to-ir (form)
  "Translate a top-level/internal `define' (function or variable)."
  (let ((target (cadr form)))
    (cond
     ((consp target)
      (let* ((name (car target)) (params (cdr target))
             (names (ldb-scheme--param-names params)))
        (ldb-ir-make 'defun
                     :form (list :name name
                                 :params (ldb-scheme--params params)
                                 :cl nil
                                 :body (let ((ldb-scheme--env (append names ldb-scheme--env)))
                                         (ldb-scheme--map-forms (cddr form))))
                     :origin 'scheme)))
     ((and (consp (caddr form)) (eq (car (caddr form)) 'lambda))
      (let* ((lam (ldb-scheme--lambda-to-ir (caddr form)))
             (lf (ldb-ir-form lam)))
        (ldb-ir-make 'defun
                     :form (list :name target
                                 :params (plist-get lf :params)
                                 :cl nil
                                 :body (plist-get lf :body))
                     :origin 'scheme)))
     (t
      (ldb-ir-make 'defvar
                   :form (list :name target
                               :value (ldb-scheme-form-to-ir (caddr form)))
                   :origin 'scheme)))))

(defun ldb-scheme--binding-irs (bindings kind)
  "Translate let BINDINGS for KIND (`let'/`let*'/`letrec') with proper scoping."
  (cond
   ((eq kind 'let)
    (mapcar (lambda (b) (list (car b) (ldb-scheme-form-to-ir (cadr b)))) bindings))
   ((eq kind 'let*)
    (let ((e ldb-scheme--env) acc)
      (dolist (b bindings (nreverse acc))
        (let ((ldb-scheme--env e))
          (push (list (car b) (ldb-scheme-form-to-ir (cadr b))) acc))
        (setq e (cons (car b) e)))))
   (t   ; letrec / letrec*: all vars visible in all inits
    (let ((ldb-scheme--env (append (mapcar #'car bindings) ldb-scheme--env)))
      (mapcar (lambda (b) (list (car b) (ldb-scheme-form-to-ir (cadr b)))) bindings)))))

(defun ldb-scheme--plain-let-to-ir (form)
  "Translate `let'/`let*'/`letrec'/`letrec*'."
  (let* ((head (car form))
         (bindings (cadr form))
         (vars (mapcar #'car bindings))
         (kind (cond ((eq head 'let*) 'let*)
                     ((memq head '(letrec letrec*)) 'letrec)
                     (t 'let))))
    (ldb-ir-make 'let
                 :form (list :kind kind
                             :star (eq kind 'let*)
                             :bindings (ldb-scheme--binding-irs bindings kind)
                             :body (let ((ldb-scheme--env (append vars ldb-scheme--env)))
                                     (ldb-scheme--map-forms (cddr form))))
                 :origin 'scheme)))

(defun ldb-scheme--named-let-to-ir (form)
  "Translate a named `let' loop to `named-let'.
Inits are evaluated in the outer env; the loop vars enter the body env;
the loop NAME stays callable directly (Emacs `named-let' semantics)."
  (let* ((name (cadr form))
         (bindings (caddr form))
         (vars (mapcar #'car bindings)))
    (ldb-ir-make 'named-let
                 :form (list :name name
                             :bindings (mapcar (lambda (b)
                                                 (list (car b) (ldb-scheme-form-to-ir (cadr b))))
                                               bindings)
                             :body (let ((ldb-scheme--env (append vars ldb-scheme--env)))
                                     (ldb-scheme--map-forms (cdddr form))))
                 :origin 'scheme)))

;;;; --- public entry ---------------------------------------------------------

;;;###autoload
(defun ldb-scheme-translate-form (form &optional globals)
  "Translate one Scheme S-expression FORM to an Elisp form (data).
Optional GLOBALS is a (FUNCS . VARS) classification; defaults to none."
  (let ((ldb-scheme--globals (or globals '(nil)))
        (ldb-scheme--env nil))
    (let ((ir (ldb-scheme-form-to-ir form)))
      (and ir (ldb-emit-elisp-node ir)))))

;;;###autoload
(defun ldb-scheme-translate-string (source)
  "Translate Scheme SOURCE (string) to a list of Elisp forms (data)."
  (let* ((forms (ldb-scheme-read-all-from-string source))
         (ldb-scheme--globals (ldb-scheme--collect-globals forms))
         (ldb-scheme--env nil))
    (delq nil (mapcar (lambda (f)
                        (let ((ir (ldb-scheme-form-to-ir f)))
                          (and ir (ldb-emit-elisp-node ir))))
                      forms))))

;;;###autoload
(defun ldb-scheme-translate-file (path)
  "Translate the Scheme file at PATH to a list of Elisp forms (data)."
  (ldb-scheme-translate-string
   (with-temp-buffer (insert-file-contents path) (buffer-string))))

(provide 'ldb-scheme-cvt)
;;; ldb-scheme-cvt.el ends here
