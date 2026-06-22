;;; ldb-cl.el --- Common Lisp core-syntax -> Elisp translator -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; This file is part of lisp-dialect-bridge.  GPL-3.0-or-later.

;;; Commentary:

;; Phase 4a: translate a *core syntax subset* of Common Lisp into
;; Elisp source (data).  Both languages are Lisp-2, so the core is
;; near-identity: read CL surface syntax, map to the dialect-neutral
;; IR, emit Elisp via `ldb-emit-elisp-node'.  Out-of-scope forms
;; (CLOS, conditions, macros, LOOP, format, packages, multiple-values,
;; quasiquote) are rejected loudly with `ldb-cl-unsupported-form-error'.
;;
;; Scope contract: docs/design/02-cl-core.org.
;;
;; Public API:
;;   (ldb-cl-translate-string SOURCE)  ; -> list of Elisp forms (data)
;;   (ldb-cl-translate-form FORM)      ; -> one Elisp form
;;   (ldb-cl-translate-file PATH)      ; -> list of Elisp forms
;;
;; Emitted code depends on cl-lib at the consumer (setf/cl-defun/
;; cl-labels/cl-incf/cl-reduce ...).

;;; Code:

(require 'cl-lib)
(require 'ldb-ir)
(require 'ldb-emit-elisp)

(define-error 'ldb-cl-unsupported-form-error
  "Common Lisp form not supported by the core translator")

;;;; --- reader ---------------------------------------------------------------

(defun ldb-cl--char-token (c)
  "Translate a Common Lisp char-literal name C (text after `#\\') to a code.
Returns the integer character code as a string, so it `read's back as
the same character (Elisp chars are integers)."
  (let ((named '(("newline" . 10) ("space" . 32) ("tab" . 9) ("return" . 13)
                 ("linefeed" . 10) ("page" . 12) ("nul" . 0) ("null" . 0)
                 ("rubout" . 127) ("delete" . 127) ("backspace" . 8)
                 ("escape" . 27) ("altmode" . 27))))
    (number-to-string
     (if (> (length c) 1)
         (or (cdr (assoc (downcase c) named)) (string-to-char c))
       (string-to-char c)))))

(defun ldb-cl--preprocess (str)
  "Convert Common Lisp surface tokens in STR to Elisp-readable form.
Handles `#| block |#' comments and `#\\X' / `#\\Newline' char literals.
Everything else (Lisp-2 symbols, keywords, t/nil, #') reads natively."
  (let ((s str))
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (while (search-forward "#|" nil t)
        (let ((start (- (point) 2)))
          (if (search-forward "|#" nil t)
              (delete-region start (point))
            (goto-char (point-max)))))
      (setq s (buffer-string)))
    (replace-regexp-in-string
     "#\\\\\\([A-Za-z][A-Za-z]+\\|.\\)"
     (lambda (m) (ldb-cl--char-token (substring m 2)))
     s t t)))

(defun ldb-cl-read-from-string (str)
  "Read the first Common Lisp expression from STR as an Elisp S-expression."
  (with-temp-buffer
    (insert (ldb-cl--preprocess str))
    (goto-char (point-min))
    (read (current-buffer))))

(defun ldb-cl-read-all-from-string (str)
  "Read every top-level Common Lisp expression from STR.  Returns a list."
  (with-temp-buffer
    (insert (ldb-cl--preprocess str))
    (goto-char (point-min))
    (let (acc)
      (condition-case nil
          (while t (push (read (current-buffer)) acc))
        (end-of-file nil)
        (invalid-read-syntax nil))
      (nreverse acc))))

(defun ldb-cl-read-file (path)
  "Read every top-level Common Lisp expression from file at PATH."
  (ldb-cl-read-all-from-string
   (with-temp-buffer (insert-file-contents path) (buffer-string))))

;;;; --- tables ---------------------------------------------------------------

(defconst ldb-cl--remap
  '((first . car) (second . cadr) (third . caddr) (fourth . cadddr) (rest . cdr)
    (incf . cl-incf) (decf . cl-decf)
    (reduce . cl-reduce) (remove . cl-remove)
    (remove-if . cl-remove-if) (remove-if-not . cl-remove-if-not)
    (delete-if . cl-delete-if) (delete-if-not . cl-delete-if-not)
    (find . cl-find) (find-if . cl-find-if) (find-if-not . cl-find-if-not)
    (position . cl-position) (position-if . cl-position-if)
    (count . cl-count) (count-if . cl-count-if)
    (every . cl-every) (some . cl-some) (notany . cl-notany) (notevery . cl-notevery)
    (member . cl-member) (member-if . cl-member-if)
    (assoc . cl-assoc) (rassoc . cl-rassoc)
    (sort . cl-sort) (stable-sort . cl-stable-sort)
    (subseq . cl-subseq) (mapcan . cl-mapcan)
    (remove-duplicates . cl-remove-duplicates)
    (block . cl-block) (return-from . cl-return-from) (return . cl-return)
    (gensym . cl-gensym)
    (evenp . cl-evenp) (oddp . cl-oddp) (plusp . cl-plusp) (minusp . cl-minusp)
    (char= . =) (char< . <) (char> . >) (char<= . <=) (char>= . >=)
    (char . aref) (schar . aref) (svref . aref)
    (char-code . identity) (code-char . identity)
    (case . cl-case) (typecase . cl-typecase) (ecase . cl-ecase)
    (etypecase . cl-etypecase) (ccase . cl-ccase))
  "Common Lisp symbol -> Elisp/cl-lib symbol for function-position heads.")

(defun ldb-cl--remap-head (sym)
  "Map a CL operator SYM to its Elisp/cl-lib equivalent (or itself)."
  (or (cdr (assq sym ldb-cl--remap)) sym))

(defconst ldb-cl--reject-heads
  '(defclass defmethod defgeneric defstruct
    define-condition handler-case handler-bind restart-case restart-bind
    defmacro macrolet symbol-macrolet define-symbol-macro define-compiler-macro
    loop do do* values multiple-value-bind multiple-value-call
    multiple-value-list multiple-value-setq nth-value
    in-package defpackage
    format formatter print princ prin1 write write-line write-string write-char
    read read-line read-char read-from-string
    with-open-file with-output-to-string open close
    tagbody go eval-when locally proclaim declaim
    destructuring-bind with-slots with-accessors prog prog*
    delay force
    \` \, \,@ quasiquote unquote unquote-splicing)
  "CL forms rejected by the core translator (signal, never silent).")

;;;; --- lambda-list analysis --------------------------------------------------

(defun ldb-cl--lambda-list-needs-cl (ll)
  "Non-nil if lambda list LL needs `cl-defun'/`cl-function' (vs plain defun)."
  (let ((needs nil) (mode nil))
    (dolist (item ll needs)
      (cond
       ((eq item '&optional) (setq mode '&optional))
       ((memq item '(&rest &body)) (setq mode '&rest))
       ((eq item '&key) (setq needs t mode '&key))
       ((eq item '&aux) (setq needs t mode '&aux))
       ((eq item '&allow-other-keys) (setq needs t))
       ((and (eq mode '&optional) (consp item)) (setq needs t))))))

(defun ldb-cl--validate-lambda-list (ll)
  "Signal if LL has a default-value expression that is not a literal/quote.
Core v1 keeps lambda-list translation trivial; complex defaults are OUT."
  (let ((mode nil))
    (dolist (item ll ll)
      (cond
       ((memq item '(&optional &rest &body &key &aux &allow-other-keys))
        (setq mode item))
       ((and (memq mode '(&optional &key)) (consp item))
        (let ((default (cadr item)))
          (when (and (consp default)
                     (not (memq (car default) '(quote function))))
            (signal 'ldb-cl-unsupported-form-error
                    (list "core v1: lambda-list defaults must be literals/quote"
                          item)))))))))

;;;; --- CL S-expression -> IR ------------------------------------------------

(defun ldb-cl-form-to-ir (form)
  "Translate a Common Lisp S-expression FORM into a dialect-neutral IR node.
Returns nil for a `declare' form (callers drop it from bodies)."
  (cond
   ((stringp form)  (ldb-ir-make 'literal :form (list :value form :type 'string)  :origin 'cl))
   ((integerp form) (ldb-ir-make 'literal :form (list :value form :type 'integer) :origin 'cl))
   ((floatp form)   (ldb-ir-make 'literal :form (list :value form :type 'float)   :origin 'cl))
   ((keywordp form) (ldb-ir-make 'literal :form (list :value form :type 'keyword) :origin 'cl))
   ((memq form '(t nil)) (ldb-ir-make 'literal :form (list :value form :type 'bool) :origin 'cl))
   ((symbolp form)  (ldb-ir-make 'ref :form (list :name form) :origin 'cl))
   ((vectorp form)
    (signal 'ldb-cl-unsupported-form-error (list "vectors unsupported in core v1" form)))
   ((consp form)    (ldb-cl--compound-to-ir form))
   (t (ldb-ir-make 'literal :form (list :value form :type 'other) :origin 'cl))))

(defun ldb-cl--map-forms (forms)
  "Translate FORMS, dropping nil results (stripped `declare')."
  (delq nil (mapcar #'ldb-cl-form-to-ir forms)))

(defun ldb-cl--compound-to-ir (form)
  "Translate a non-empty list FORM by dispatching on its head."
  (let ((head (car form)))
    (cond
     ((not (symbolp head))
      (signal 'ldb-cl-unsupported-form-error
              (list "non-symbol operator unsupported in core v1" form)))
     ((memq head ldb-cl--reject-heads)
      (signal 'ldb-cl-unsupported-form-error
              (list (format "%S unsupported by CL core translator" head) form)))
     ((eq head 'quote) (ldb-ir-make 'quote :form (list :datum (cadr form)) :origin 'cl))
     ((eq head 'declare) nil)
     ((eq head 'the) (ldb-cl-form-to-ir (nth 2 form)))
     ((eq head 'if)
      (ldb-ir-make 'if
                   :form (list :cond (ldb-cl-form-to-ir (nth 1 form))
                               :then (ldb-cl-form-to-ir (nth 2 form))
                               :else (when (> (length form) 3)
                                       (ldb-cl-form-to-ir (nth 3 form))))
                   :origin 'cl))
     ((eq head 'cond) (ldb-cl--cond-to-ir form))
     ((memq head '(let let*)) (ldb-cl--let-to-ir form))
     ((eq head 'lambda) (ldb-cl--lambda-to-ir form))
     ((eq head 'defun) (ldb-cl--defun-to-ir form))
     ((memq head '(defvar defparameter defconstant)) (ldb-cl--defvar-to-ir form))
     ((memq head '(case typecase ecase etypecase ccase)) (ldb-cl--case-to-ir form))
     ((memq head '(dolist dotimes)) (ldb-cl--bind-to-ir form))
     ((memq head '(labels flet)) (ldb-cl--locals-to-ir form))
     (t
      (ldb-ir-make 'call
                   :form (list :fn (ldb-cl--remap-head head)
                               :args (ldb-cl--map-forms (cdr form)))
                   :origin 'cl)))))

(defun ldb-cl--cond-to-ir (form)
  "Translate a `cond' FORM."
  (ldb-ir-make 'cond
               :form (list :clauses
                           (mapcar
                            (lambda (clause)
                              (unless (consp clause)
                                (signal 'ldb-cl-unsupported-form-error
                                        (list "cond clause must be a list" clause)))
                              (list (ldb-cl-form-to-ir (car clause))
                                    (ldb-cl--map-forms (cdr clause))))
                            (cdr form)))
               :origin 'cl))

(defun ldb-cl--binding-pair (b)
  "Translate one let binding B (`(VAR VAL)' / `(VAR)' / VAR)."
  (cond
   ((symbolp b) (list b))
   ((and (consp b) (symbolp (car b)))
    (if (cdr b) (list (car b) (ldb-cl-form-to-ir (cadr b))) (list (car b))))
   (t (signal 'ldb-cl-unsupported-form-error (list "bad let binding" b)))))

(defun ldb-cl--let-to-ir (form)
  "Translate a `let'/`let*' FORM."
  (ldb-ir-make 'let
               :form (list :star (eq (car form) 'let*)
                           :bindings (mapcar #'ldb-cl--binding-pair (cadr form))
                           :body (ldb-cl--map-forms (cddr form)))
               :origin 'cl))

(defun ldb-cl--lambda-to-ir (form)
  "Translate a `lambda' FORM."
  (let ((ll (cadr form)))
    (ldb-cl--validate-lambda-list ll)
    (ldb-ir-make 'lambda
                 :form (list :params ll
                             :body (ldb-cl--map-forms (cddr form))
                             :cl (ldb-cl--lambda-list-needs-cl ll))
                 :origin 'cl)))

(defun ldb-cl--defun-to-ir (form)
  "Translate a `defun' FORM."
  (let ((ll (nth 2 form)))
    (ldb-cl--validate-lambda-list ll)
    (ldb-ir-make 'defun
                 :form (list :name (nth 1 form) :params ll
                             :body (ldb-cl--map-forms (cdddr form))
                             :cl (ldb-cl--lambda-list-needs-cl ll))
                 :origin 'cl)))

(defun ldb-cl--defvar-to-ir (form)
  "Translate a `defvar'/`defparameter' FORM (defparameter folds to defvar)."
  (ldb-ir-make 'defvar
               :form (list :name (nth 1 form)
                           :value (when (> (length form) 2)
                                    (ldb-cl-form-to-ir (nth 2 form)))
                           :kind (car form))
               :origin 'cl))

(defun ldb-cl--bind-to-ir (form)
  "Translate a `dolist'/`dotimes' FORM."
  (let* ((spec (cadr form)))
    (ldb-ir-make 'bind
                 :form (list :head (car form)
                             :var (nth 0 spec)
                             :iter (ldb-cl-form-to-ir (nth 1 spec))
                             :result (when (nth 2 spec) (ldb-cl-form-to-ir (nth 2 spec)))
                             :body (ldb-cl--map-forms (cddr form)))
                 :origin 'cl)))

(defun ldb-cl--case-to-ir (form)
  "Translate a `case'/`typecase'/`ecase' FORM.
Clause keys are DATA (kept verbatim); clause bodies are code."
  (ldb-ir-make 'case
               :form (list :head (ldb-cl--remap-head (car form))
                           :key (ldb-cl-form-to-ir (nth 1 form))
                           :clauses (mapcar
                                     (lambda (clause)
                                       (cons (car clause)
                                             (ldb-cl--map-forms (cdr clause))))
                                     (cddr form)))
               :origin 'cl))

(defun ldb-cl--locals-to-ir (form)
  "Translate a `labels'/`flet' FORM."
  (ldb-ir-make 'locals
               :form (list :head (car form)
                           :defs (mapcar
                                  (lambda (d)
                                    (let ((ll (nth 1 d)))
                                      (ldb-cl--validate-lambda-list ll)
                                      (list (nth 0 d) ll (ldb-cl--map-forms (cddr d)))))
                                  (cadr form))
                           :body (ldb-cl--map-forms (cddr form)))
               :origin 'cl))

;;;; --- public entry ---------------------------------------------------------

;;;###autoload
(defun ldb-cl-translate-form (form)
  "Translate one Common Lisp S-expression FORM to an Elisp form (data)."
  (let ((ir (ldb-cl-form-to-ir form)))
    (and ir (ldb-emit-elisp-node ir))))

;;;###autoload
(defun ldb-cl-translate-string (source)
  "Translate Common Lisp SOURCE (string) to a list of Elisp forms (data)."
  (delq nil (mapcar #'ldb-cl-translate-form (ldb-cl-read-all-from-string source))))

;;;###autoload
(defun ldb-cl-translate-file (path)
  "Translate the Common Lisp file at PATH to a list of Elisp forms (data)."
  (delq nil (mapcar #'ldb-cl-translate-form (ldb-cl-read-file path))))

(provide 'ldb-cl)
;;; ldb-cl.el ends here
