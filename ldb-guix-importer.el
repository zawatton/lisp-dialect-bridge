;;; ldb-guix-importer.el --- Phase 1 Guix recipe -> pkg-define entry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; This file is part of lisp-dialect-bridge.  GPL-3.0-or-later.

;;; Commentary:

;; Phase 1 entry point.  Reads a Guix recipe (Scheme source) and
;; emits an anvil-pkg `pkg-define' form.  The output is data — the
;; caller decides what to do with it (pp / write-file / eval).
;;
;; Public API:
;;   (ldb-guix-import-string SOURCE NAME-SYM)  ; from string
;;   (ldb-guix-import-file FILE NAME-SYM)      ; from file
;;   (ldb-guix-import-form FORM)               ; from already-read S-expression
;;
;; Phase 1 reject list — any of these signal `ldb-unsupported-guix-form-error':
;;   - quasiquote `(...)` in inputs (use `(list ...)' instead)
;;   - `arguments' field with substitute* / phase patches
;;   - `package-inherit' / `inherit-from'
;;   - cross-compilation declarations
;;   - any package field outside the L5 mapping table

;;; Code:

(require 'cl-lib)
(require 'ldb-ir)
(require 'ldb-scheme)
(require 'ldb-emit-elisp)

(define-error 'ldb-unsupported-guix-form-error
              "Guix form not supported by Phase 1 importer")

;;;; --- public entry --------------------------------------------------------

(defun ldb-guix-import-string (source name-sym)
  "Translate Guix recipe SOURCE (string) for NAME-SYM into a `pkg-define' form."
  (let* ((forms (ldb-scheme-read-all-from-string source))
         (target (ldb--guix-find-define-public forms name-sym))
         (pkg-form (caddr target))
         (ir (ldb--guix-form-to-ir pkg-form)))
    (ldb-emit-pkg-define ir name-sym)))

(defun ldb-guix-import-file (path name-sym)
  "Translate Guix recipe at PATH for NAME-SYM into a `pkg-define' form."
  (ldb-guix-import-string
   (with-temp-buffer
     (insert-file-contents path)
     (buffer-string))
   name-sym))

(defun ldb-guix-import-form (form)
  "Translate a single (define-public NAME (package ...)) FORM."
  (cond
   ((and (consp form) (eq (car form) 'define-public))
    (let* ((name (cadr form))
           (pkg-form (caddr form))
           (ir (ldb--guix-form-to-ir pkg-form)))
      (ldb-emit-pkg-define ir name)))
   (t (signal 'ldb-unsupported-guix-form-error
              (list (format "expected (define-public NAME (package ...)), got %S"
                            form))))))

;;;; --- internal: find ------------------------------------------------------

(defun ldb--guix-find-define-public (forms name-sym)
  "Locate `(define-public NAME-SYM ...)' in FORMS, signal if absent."
  (or (cl-find-if (lambda (f)
                    (and (consp f)
                         (eq (car f) 'define-public)
                         (eq (cadr f) name-sym)))
                  forms)
      (signal 'ldb-unsupported-guix-form-error
              (list (format "no (define-public %s ...) in source" name-sym)))))

;;;; --- internal: Scheme S-expression -> IR ---------------------------------

(defun ldb--guix-form-to-ir (form)
  "Convert a Scheme S-expression FORM into an IR node."
  (cond
   ;; literals
   ((stringp form)
    (ldb-ir-make 'literal :form (list :value form :type 'string)
                 :origin 'scheme))
   ((numberp form)
    (ldb-ir-make 'literal :form (list :value form :type 'number)
                 :origin 'scheme))
   ;; symbols -> ref
   ((symbolp form)
    (ldb-ir-make 'ref :form (list :name form) :origin 'scheme))
   ;; quasiquote / unquote — Phase 1 reject (use (list ...) instead)
   ((and (consp form) (memq (car form) '(\` quasiquote \, unquote \,@ unquote-splicing)))
    (signal 'ldb-unsupported-guix-form-error
            (list "quasiquote / unquote is not supported by Phase 1 — use (list ...) for inputs")))
   ;; well-known shapes
   ((and (consp form) (eq (car form) 'package))
    (ldb-ir-make 'record
                 :form (list :type 'package
                             :fields (mapcar #'ldb--guix-field-to-ir (cdr form)))
                 :origin 'scheme))
   ((and (consp form) (eq (car form) 'origin))
    (ldb-ir-make 'record
                 :form (list :type 'origin
                             :fields (mapcar #'ldb--guix-field-to-ir (cdr form)))
                 :origin 'scheme))
   ((and (consp form) (eq (car form) 'git-reference))
    (ldb-ir-make 'record
                 :form (list :type 'git-reference
                             :fields (mapcar #'ldb--guix-field-to-ir (cdr form)))
                 :origin 'scheme))
   ;; (base32 H) — collapse to a literal carrying H.
   ;; (sha256 ...) is processed by `ldb--guix-field-to-ir' as a field,
   ;; whose value passes through here — the (base32 H) call thus
   ;; reduces to the bare hash literal that the emitter expects.
   ((and (consp form) (eq (car form) 'base32))
    (ldb-ir-make 'literal
                 :form (list :value (cadr form) :type 'base32-string)
                 :origin 'scheme))
   ;; (list a b c) — input list
   ((and (consp form) (eq (car form) 'list))
    (ldb-ir-make 'list-of
                 :form (list :items (mapcar #'ldb--guix-form-to-ir (cdr form)))
                 :origin 'scheme))
   (t (signal 'ldb-unsupported-guix-form-error
              (list (format "Phase 1 cannot convert form %S" form))))))

(defun ldb--guix-field-to-ir (field-form)
  "Convert one record field `(NAME VALUE)' into a `(NAME IR-NODE)' tuple."
  (unless (and (consp field-form)
               (= 2 (length field-form))
               (symbolp (car field-form)))
    (signal 'ldb-unsupported-guix-form-error
            (list (format "field must be (NAME VALUE), got %S" field-form))))
  (list (car field-form) (ldb--guix-form-to-ir (cadr field-form))))

(provide 'ldb-guix-importer)
;;; ldb-guix-importer.el ends here
