;;; ldb-scheme-macro.el --- Scheme define-syntax pre-expansion via Guile -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5: Scheme `define-syntax' (hygienic macros) support by
;; pre-expansion through an external GNU Guile, the Scheme analogue of the
;; CL/SBCL path in `ldb-cl-macro.el'.  Scheme macros are harder than CL
;; here: Guile's `macroexpand' lowers all the way to Tree-IL, not readable
;; Scheme.  The trick is `tree-il->scheme', which reconstructs *readable*
;; Scheme that stays in the translator's subset (it keeps high-level forms
;; like let / cond / and / define and only renames identifiers where
;; hygiene actually requires it):
;;
;;   (swap! tmp other)  --macroexpand-->  Tree-IL  --tree-il->scheme-->
;;     (let ((tmp-1 tmp)) (set! tmp other) (set! other tmp-1))
;;
;; i.e. hygiene is handled by Guile (the macro's `tmp' is renamed to avoid
;; capturing the caller's `tmp'), and the renamed identifier round-trips as
;; an ordinary symbol the bridge translates.  The Guile walker evaluates
;; the `define-syntax' forms (and `define's) so uses can expand, drops the
;; `define-syntax' / module forms from the output, and emits each remaining
;; form through `tree-il->scheme (macroexpand FORM)'.  The expanded Scheme
;; then goes to the normal `ldb-scheme-translate-*'.
;;
;; Gated on Guile on PATH; correctness is the same differential discipline
;; (run the macro program in Guile vs pre-expand+translate+run here).

;;; Code:

(require 'ldb-scheme-cvt)

(define-error 'ldb-scheme-macro-error "Scheme macro pre-expansion failed")

(defgroup ldb-scheme-macro nil
  "Scheme macro pre-expansion for lisp-dialect-bridge."
  :group 'tools)

(defcustom ldb-scheme-guile-program
  (or (executable-find "guile")
      (let ((p "/usr/bin/guile")) (and (file-executable-p p) p)))
  "Path to GNU Guile, used for Scheme macro pre-expansion (and as the
Scheme differential oracle).  When nil, expansion signals
`ldb-scheme-macro-error'."
  :type '(choice (const :tag "Not available" nil) file)
  :group 'ldb-scheme-macro)

(defconst ldb-scheme-macro--walker "\
(use-modules (language tree-il))

(define (read-all port)
  (let loop ((acc '()))
    (let ((f (read port)))
      (if (eof-object? f) (reverse acc) (loop (cons f acc))))))

(define (eval-pass1? f)
  (and (pair? f)
       (memq (car f) '(define-syntax define-syntax-rule define define-values))))

(define (drop-output? f)
  (and (pair? f)
       (memq (car f) '(define-syntax define-syntax-rule use-modules import include))))

(define (process path)
  (let ((forms (call-with-input-file path read-all)))
    (for-each (lambda (f)
                (when (eval-pass1? f)
                  (catch #t
                    (lambda () (primitive-eval f))
                    (lambda (k . a)
                      (format (current-error-port) \"pass1: ~a~%\" k)))))
              forms)
    (let ((out (open-output-string)))
      (for-each (lambda (f)
                  (unless (drop-output? f)
                    (write (tree-il->scheme (macroexpand f)) out)
                    (newline out)))
                forms)
      (display (get-output-string out)))))

(catch #t
  (lambda () (process (cadr (command-line))))
  (lambda (k . a)
    (format (current-error-port) \"preexpand: ~a ~a~%\" k a)
    (exit 3)))
"
  "Guile script: expand user macros to readable Scheme via tree-il->scheme.
Reads the Scheme source path from the command line, evaluates the
`define-syntax' / `define' forms, drops `define-syntax' / module forms,
and writes each remaining form macro-expanded back to readable Scheme.")

;;;###autoload
(defun ldb-scheme-macro-preexpand-available-p ()
  "Return the Guile program path when macro pre-expansion is possible, else nil."
  (and ldb-scheme-guile-program (file-executable-p ldb-scheme-guile-program)
       ldb-scheme-guile-program))

;;;###autoload
(defun ldb-scheme-macro-expand-string (source)
  "Pre-expand `define-syntax' macros in Scheme SOURCE (string) via Guile.
Return the expanded Scheme source as a string (macro uses inlined,
`define-syntax' definitions dropped).  Signal `ldb-scheme-macro-error'
when Guile is unavailable or pre-expansion fails."
  (let ((guile (ldb-scheme-macro-preexpand-available-p)))
    (unless guile
      (signal 'ldb-scheme-macro-error
              (list "Guile not available for macro pre-expansion")))
    (let ((walker (make-temp-file "ldb-gwalker-" nil ".scm"))
          (src (make-temp-file "ldb-gsrc-" nil ".scm"))
          (errfile (make-temp-file "ldb-gerr-" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file walker (insert ldb-scheme-macro--walker))
            (with-temp-file src (insert source))
            (with-temp-buffer
              (let ((code (call-process guile nil (list (current-buffer) errfile) nil
                                        "--no-auto-compile" "-s" walker src)))
                (unless (eq code 0)
                  (signal 'ldb-scheme-macro-error
                          (list (format "Guile pre-expansion failed (exit %s): %s"
                                        code
                                        (with-temp-buffer
                                          (insert-file-contents errfile)
                                          (string-trim (buffer-string)))))))
                (buffer-string))))
        (delete-file walker)
        (delete-file src)
        (delete-file errfile)))))

;;;###autoload
(defun ldb-scheme-translate-string-expanding (source)
  "Translate Scheme SOURCE to Elisp forms, pre-expanding `define-syntax'.
Runs SOURCE through `ldb-scheme-macro-expand-string' (Guile) first, then
the normal `ldb-scheme-translate-string'."
  (ldb-scheme-translate-string (ldb-scheme-macro-expand-string source)))

;;;###autoload
(defun ldb-scheme-translate-file-expanding (path)
  "Translate the Scheme file at PATH to Elisp forms, pre-expanding macros."
  (ldb-scheme-translate-string-expanding
   (with-temp-buffer (insert-file-contents path) (buffer-string))))

(provide 'ldb-scheme-macro)
;;; ldb-scheme-macro.el ends here
