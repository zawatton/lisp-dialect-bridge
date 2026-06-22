;;; ldb-emit-elisp.el --- IR -> Elisp (pkg-define) emitter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; This file is part of lisp-dialect-bridge.  GPL-3.0-or-later.

;;; Commentary:

;; Phase 1 emitter.  Takes a `package' record IR node and produces an
;; anvil-pkg `pkg-define' form (as data — caller pp / write / eval).
;;
;; Mappings:
;;   record :type 'package           -> (pkg-define ...)
;;   record :type 'origin (url-fetch) -> (source (url-fetch URL :sha256 H))
;;   record :type 'origin (git-fetch) -> (source (git-fetch :url ... :rev ... :sha256 ...))
;;   ref stdenv-build-system          -> (build-system stdenv)
;;   ref cargo-build-system           -> (build-system (rust))   ; cargo-sha256 left unfilled
;;   ref python-build-system          -> (build-system python)
;;   ref go-build-system              -> (build-system go)
;;   list-of refs (inputs)            -> (list ...)
;;   ref license:gpl3+ etc.           -> (license gpl3)         ; prefix/suffix stripped
;;   synopsis                         -> description
;;   home-page                        -> homepage

;;; Code:

(require 'ldb-ir)

(define-error 'ldb-emit-error "anvil-pkg emit error")

;;;; --- public entry ---------------------------------------------------------

(defun ldb-emit-pkg-define (record-ir name-sym)
  "Emit a `pkg-define' S-expression from a `package' RECORD-IR using NAME-SYM.

Returns an S-expression (Lisp data) suitable for `pp', `prin1' or
direct `eval'.  Does not write to a file; the caller decides where
the output goes."
  (unless (and (ldb-ir-record-p record-ir)
               (eq 'package (ldb-ir-record-type record-ir)))
    (signal 'ldb-emit-error
            (list (format "expected package record, got %S" record-ir))))
  (let ((subforms '()))
    ;; version
    (when-let ((v (ldb-ir-record-field record-ir 'version)))
      (push (list 'version (ldb--literal-value v)) subforms))
    ;; source (origin)
    (when-let ((s (ldb-ir-record-field record-ir 'source)))
      (push (list 'source (ldb--emit-source s)) subforms))
    ;; build-system
    (when-let ((bs (ldb-ir-record-field record-ir 'build-system)))
      (push (list 'build-system (ldb--emit-build-system bs)) subforms))
    ;; inputs / native-inputs
    (when-let ((in (ldb-ir-record-field record-ir 'inputs)))
      (push (list 'inputs (ldb--emit-input-list in)) subforms))
    (when-let ((nin (ldb-ir-record-field record-ir 'native-inputs)))
      (push (list 'native-inputs (ldb--emit-input-list nin)) subforms))
    ;; synopsis -> description (Phase 1 mapping per design L5)
    (when-let ((syn (ldb-ir-record-field record-ir 'synopsis)))
      (push (list 'description (ldb--literal-value syn)) subforms))
    ;; home-page -> homepage
    (when-let ((hp (ldb-ir-record-field record-ir 'home-page)))
      (push (list 'homepage (ldb--literal-value hp)) subforms))
    ;; license
    (when-let ((lic (ldb-ir-record-field record-ir 'license)))
      (push (list 'license (ldb--emit-license lic)) subforms))
    `(pkg-define ,name-sym ,@(nreverse subforms))))

;;;; --- field-specific emitters ----------------------------------------------

(defun ldb--literal-value (node)
  "Return the underlying value of literal NODE; signal otherwise."
  (unless (ldb-ir-literal-p node)
    (signal 'ldb-emit-error (list (format "expected literal, got %S" node))))
  (ldb-ir-literal-value node))

(defun ldb--emit-source (src-node)
  "Convert an origin record IR node into the inline Elisp source form."
  (unless (and (ldb-ir-record-p src-node)
               (eq 'origin (ldb-ir-record-type src-node)))
    (signal 'ldb-emit-error (list "source must be an origin record")))
  (let* ((method-node (ldb-ir-record-field src-node 'method))
         (method (and method-node (ldb-ir-ref-name method-node))))
    (pcase method
      ('url-fetch
       (let ((uri (ldb--literal-value
                   (ldb-ir-record-field src-node 'uri)))
             (sha (ldb--unwrap-sha256
                   (ldb-ir-record-field src-node 'sha256))))
         `(url-fetch ,uri :sha256 ,sha)))
      ('git-fetch
       (let* ((uri-node (ldb-ir-record-field src-node 'uri))
              (url (ldb--literal-value
                    (ldb-ir-record-field uri-node 'url)))
              (commit (ldb--literal-value
                       (ldb-ir-record-field uri-node 'commit)))
              (sha (ldb--unwrap-sha256
                    (ldb-ir-record-field src-node 'sha256))))
         `(git-fetch :url ,url :rev ,commit :sha256 ,sha)))
      (_ (signal 'ldb-emit-error
                 (list (format "unsupported origin method %S (Phase 1: url-fetch / git-fetch)"
                               method)))))))

(defun ldb--unwrap-sha256 (sha-node)
  "Extract hash string from the IR node stored under a `sha256' field.

Phase 1 simplification: the field handler reduces
`(sha256 (base32 H))' to a literal carrying H directly, so the
emitter only sees a literal here.  Anything else signals."
  (cond
   ((ldb-ir-literal-p sha-node)
    (ldb-ir-literal-value sha-node))
   (t (signal 'ldb-emit-error
              (list (format "sha256 must be (base32 STRING); got IR %S"
                            sha-node))))))

(defconst ldb--build-system-map
  '((stdenv-build-system . stdenv)
    (cargo-build-system  . (rust))
    (python-build-system . python)
    (go-build-system     . go))
  "Phase 1 Guix build-system symbol -> anvil-pkg build-system value.

`cargo-build-system' maps to the bare list `(rust)' because anvil-pkg
requires `:cargo-sha256' which Phase 1 cannot derive from a Guix
recipe — the user must add it after import.")

(defun ldb--emit-build-system (bs-node)
  "Convert a build-system ref IR node into the Elisp form."
  (unless (ldb-ir-ref-p bs-node)
    (signal 'ldb-emit-error (list "build-system must be a symbol ref")))
  (let* ((sym (ldb-ir-ref-name bs-node))
         (mapped (assq sym ldb--build-system-map)))
    (if mapped
        (cdr mapped)
      (signal 'ldb-emit-error
              (list (format "Phase 1 unsupported build-system %S; supported: %S"
                            sym (mapcar #'car ldb--build-system-map)))))))

(defun ldb--emit-input-list (in-node)
  "Convert a list-of IR node into a `(list S1 S2 ...)' Elisp form."
  (unless (ldb-ir-list-of-p in-node)
    (signal 'ldb-emit-error
            (list "inputs/native-inputs must use (list ...) form (Phase 1 limit)")))
  (let* ((items (plist-get (ldb-ir-form in-node) :items))
         (syms (mapcar
                (lambda (it)
                  (unless (ldb-ir-ref-p it)
                    (signal 'ldb-emit-error
                            (list (format "input list element must be symbol, got %S" it))))
                  (ldb-ir-ref-name it))
                items)))
    `(list ,@syms)))

(defconst ldb--license-name-map
  '(("expat" . "mit")
    ("bsd-3" . "bsd3")
    ("bsd-2" . "bsd2")
    ("asl2.0" . "apache2"))
  "Guix-flavoured license short-name -> anvil-pkg short-name.")

(defun ldb--emit-license (lic-node)
  "Convert a license ref node like `license:gpl3+' to anvil-pkg's `gpl3'."
  (unless (ldb-ir-ref-p lic-node)
    (signal 'ldb-emit-error (list "license must be a symbol ref")))
  (let* ((sym (ldb-ir-ref-name lic-node))
         (str (symbol-name sym))
         (clean (replace-regexp-in-string "\\`license:" "" str))
         (clean (replace-regexp-in-string "[+]\\'" "" clean))
         (mapped (cdr (assoc clean ldb--license-name-map))))
    (intern (or mapped clean))))

;;;; --- general IR -> Elisp node emitter (Phase 4a: CL core) ----------------

;; Dialect-neutral: consumes the general IR tags (01-overview L2 plus the
;; defun/defvar/cond/bind/locals tags added in 02-cl-core).  All "head +
;; expression args" forms (function calls AND elisp-identical special forms
;; like when/and/setq/setf/block) share the `call' tag — emission is just
;; `(head . emitted-args)', so they need no per-form code here.

(defun ldb-emit-elisp--lambda-list (ll)
  "Translate a (mostly CL) lambda list LL to an Elisp lambda list."
  (mapcar (lambda (x) (if (eq x '&body) '&rest x)) ll))

(defun ldb-emit-elisp--bq-template (tmpl)
  "Rebuild a backquote template TMPL, emitting Elisp for unquoted IR nodes."
  (cond
   ((and (consp tmpl) (eq (car tmpl) 'ldb-unquote))
    (list ldb-ir-unquote-symbol (ldb-emit-elisp-node (cadr tmpl))))
   ((and (consp tmpl) (eq (car tmpl) 'ldb-splice))
    (list ldb-ir-splice-symbol (ldb-emit-elisp-node (cadr tmpl))))
   ((consp tmpl)
    (cons (ldb-emit-elisp--bq-template (car tmpl))
          (ldb-emit-elisp--bq-template (cdr tmpl))))
   (t tmpl)))

(defun ldb-emit-elisp--hc-clause (clause var)
  "Emit a condition-case handler from CLAUSE (COND CVAR BODY-IR) using VAR.
A per-clause var different from VAR is re-bound with `let'."
  (let ((cond-name (nth 0 clause))
        (cvar (nth 1 clause))
        (body (mapcar #'ldb-emit-elisp-node (nth 2 clause))))
    (cons cond-name
          (if (and cvar (not (eq cvar var)))
              (list (append (list 'let (list (list cvar var))) body))
            body))))

(defun ldb-emit-elisp--clos-slot (slot)
  "Rebuild a defclass SLOT, emitting Elisp for an IR-node :initform value."
  (if (symbolp slot) slot
    (cons (car slot)
          (mapcar (lambda (v) (if (and (consp v) (eq (car v) :tag))
                                  (ldb-emit-elisp-node v)
                                v))
                  (cdr slot)))))

(defun ldb-emit-elisp-node (node)
  "Emit Elisp source (as data) from a general IR NODE."
  (pcase (ldb-ir-tag node)
    ('literal (ldb-ir-literal-value node))
    ('ref (plist-get (ldb-ir-form node) :name))
    ('quote (list 'quote (plist-get (ldb-ir-form node) :datum)))
    ('call
     (cons (plist-get (ldb-ir-form node) :fn)
           (mapcar #'ldb-emit-elisp-node (plist-get (ldb-ir-form node) :args))))
    ('if
     (let ((f (ldb-ir-form node)))
       (append (list 'if
                     (ldb-emit-elisp-node (plist-get f :cond))
                     (ldb-emit-elisp-node (plist-get f :then)))
               (when (plist-get f :else)
                 (list (ldb-emit-elisp-node (plist-get f :else)))))))
    ('cond
     (cons 'cond
           (mapcar (lambda (clause)
                     (cons (ldb-emit-elisp-node (car clause))
                           (mapcar #'ldb-emit-elisp-node (cadr clause))))
                   (plist-get (ldb-ir-form node) :clauses))))
    ('let
     (let* ((f (ldb-ir-form node))
            ;; CL sets :star only; Scheme sets :kind let/let*/letrec.
            (kw (or (plist-get f :kind) (if (plist-get f :star) 'let* 'let))))
       (append (list kw
                     (mapcar (lambda (b)
                               (if (cdr b)
                                   (list (car b) (ldb-emit-elisp-node (cadr b)))
                                 (list (car b))))
                             (plist-get f :bindings)))
               (mapcar #'ldb-emit-elisp-node (plist-get f :body)))))
    ('named-let
     (let ((f (ldb-ir-form node)))
       (append (list 'named-let (plist-get f :name)
                     (mapcar (lambda (b)
                               (list (car b) (ldb-emit-elisp-node (cadr b))))
                             (plist-get f :bindings)))
               (mapcar #'ldb-emit-elisp-node (plist-get f :body)))))
    ('lambda
     (let* ((f (ldb-ir-form node))
            (core (append (list 'lambda
                                (ldb-emit-elisp--lambda-list (plist-get f :params)))
                          (mapcar #'ldb-emit-elisp-node (plist-get f :body)))))
       (if (plist-get f :cl) (list 'cl-function core) core)))
    ('defun
     (let* ((f (ldb-ir-form node))
            (kw (if (plist-get f :cl) 'cl-defun 'defun)))
       (append (list kw (plist-get f :name)
                     (ldb-emit-elisp--lambda-list (plist-get f :params)))
               (mapcar #'ldb-emit-elisp-node (plist-get f :body)))))
    ('defvar
     (let ((f (ldb-ir-form node)))
       (append (list 'defvar (plist-get f :name))
               (when (plist-get f :value)
                 (list (ldb-emit-elisp-node (plist-get f :value)))))))
    ('bind
     (let* ((f (ldb-ir-form node))
            (spec (append (list (plist-get f :var)
                                (ldb-emit-elisp-node (plist-get f :iter)))
                          (when (plist-get f :result)
                            (list (ldb-emit-elisp-node (plist-get f :result)))))))
       (append (list (plist-get f :head) spec)
               (mapcar #'ldb-emit-elisp-node (plist-get f :body)))))
    ('locals
     (let* ((f (ldb-ir-form node))
            (head (if (eq (plist-get f :head) 'labels) 'cl-labels 'cl-flet)))
       (append (list head
                     (mapcar (lambda (d)
                               (append (list (nth 0 d)
                                             (ldb-emit-elisp--lambda-list (nth 1 d)))
                                       (mapcar #'ldb-emit-elisp-node (nth 2 d))))
                             (plist-get f :defs)))
               (mapcar #'ldb-emit-elisp-node (plist-get f :body)))))
    ('backquote
     (list ldb-ir-backquote-symbol
           (ldb-emit-elisp--bq-template (plist-get (ldb-ir-form node) :template))))
    ('handler-case
     (let* ((f (ldb-ir-form node))
            (var (plist-get f :var)))
       (append (list 'condition-case var
                     (ldb-emit-elisp-node (plist-get f :protected)))
               (mapcar (lambda (cl) (ldb-emit-elisp--hc-clause cl var))
                       (plist-get f :clauses)))))
    ('defclass
     (let ((f (ldb-ir-form node)))
       (append (list 'defclass (plist-get f :name) (plist-get f :supers)
                     (mapcar #'ldb-emit-elisp--clos-slot (plist-get f :slots)))
               (plist-get f :options))))
    ('defmethod
     (let ((f (ldb-ir-form node)))
       (append (list 'cl-defmethod (plist-get f :name))
               (and (plist-get f :qualifier) (list (plist-get f :qualifier)))
               (list (plist-get f :arglist))
               (mapcar #'ldb-emit-elisp-node (plist-get f :body)))))
    ('defgeneric
     (let ((f (ldb-ir-form node)))
       (append (list 'cl-defgeneric (plist-get f :name) (plist-get f :args))
               (plist-get f :options))))
    ('loop
     (cons 'cl-loop
           (mapcar (lambda (el)
                     (if (and (consp el) (eq (car el) :tag))
                         (ldb-emit-elisp-node el)
                       el))
                   (plist-get (ldb-ir-form node) :clauses))))
    ('do
     (let* ((f (ldb-ir-form node))
            (kw (if (plist-get f :star) 'cl-do* 'cl-do))
            (bindings (mapcar
                       (lambda (b)
                         (cond
                          ((null (cdr b)) (list (car b)))
                          ((null (cddr b))
                           (list (car b) (ldb-emit-elisp-node (cadr b))))
                          (t (list (car b)
                                   (ldb-emit-elisp-node (cadr b))
                                   (ldb-emit-elisp-node (caddr b))))))
                       (plist-get f :bindings)))
            (end (cons (ldb-emit-elisp-node (plist-get f :end-test))
                       (mapcar #'ldb-emit-elisp-node (plist-get f :results)))))
       (append (list kw bindings end)
               (mapcar #'ldb-emit-elisp-node (plist-get f :body)))))
    ('mvb
     (let ((f (ldb-ir-form node)))
       (append (list 'cl-multiple-value-bind (plist-get f :vars)
                     (ldb-emit-elisp-node (plist-get f :value)))
               (mapcar #'ldb-emit-elisp-node (plist-get f :body)))))
    ('format
     (let* ((f (ldb-ir-form node))
            (call (cons 'format (cons (plist-get f :control)
                                      (mapcar #'ldb-emit-elisp-node (plist-get f :args))))))
       (if (plist-get f :dest) (list 'princ call) call)))
    ('defstruct
     (let ((f (ldb-ir-form node)))
       (append (list 'cl-defstruct (plist-get f :spec))
               (when (plist-get f :doc) (list (plist-get f :doc)))
               (mapcar (lambda (s)
                         (if (symbolp s) s
                           (cons (car s)
                                 (cons (ldb-emit-elisp-node (cadr s)) (cddr s)))))
                       (plist-get f :slots)))))
    ('case
     (let ((f (ldb-ir-form node)))
       (append (list (plist-get f :head) (ldb-emit-elisp-node (plist-get f :key)))
               (mapcar (lambda (clause)
                         (cons (car clause)
                               (mapcar #'ldb-emit-elisp-node (cdr clause))))
                       (plist-get f :clauses)))))
    ('module
     (cons 'progn (mapcar #'ldb-emit-elisp-node
                          (plist-get (ldb-ir-form node) :items))))
    (_ (signal 'ldb-emit-error
               (list (format "ldb-emit-elisp-node: unhandled IR tag %S"
                             (ldb-ir-tag node)))))))

(provide 'ldb-emit-elisp)
;;; ldb-emit-elisp.el ends here
