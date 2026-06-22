;;; ldb-cl-macro.el --- CL macro pre-expansion via external SBCL -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 4e: Common Lisp `defmacro' support by SELECTIVE pre-expansion
;; through an external SBCL, rather than translating macros directly (the
;; hard hygiene problem).  See `docs/design/01-overview.org' L3 for the
;; locked strategy.  In short:
;;
;;   * An SBCL code-walker expands only USER macros (operators whose
;;     `macro-function' is non-nil and whose home package is not
;;     COMMON-LISP), one `macroexpand-1' step at a time, recursing into the
;;     result.  Standard CL macros (`when', `loop', `defun', `case', ...)
;;     are left intact for the existing 4a-4d' handlers.  Full
;;     `macroexpand-all' is deliberately NOT used — it would explode
;;     standard macros into block/tagbody/go primitives the translator does
;;     not cover.
;;   * `defmacro' forms are evaluated into SBCL (so their uses can expand)
;;     then dropped from the output; only the expansions survive.
;;   * Output is printed with `*print-case* :downcase' (and no length/level
;;     truncation) so SBCL's upcased symbols round-trip to the lowercase CL
;;     symbols Emacs `read' / the bridge dispatch on.
;;
;; The expanded source is then handed to the normal `ldb-cl-translate-*'.
;; Correctness is gated by the SBCL differential tests: the same program run
;; in SBCL and through (pre-expand -> translate -> eval) must agree.
;;
;; Out of scope (v1): `macrolet' / `symbol-macrolet' (local macros — the
;; walker uses only the global `macro-function'), compiler-macros, and read
;; macros beyond the reader's `#\\' / `#'' / `#|...|#' handling.

;;; Code:

(require 'ldb-cl)

(define-error 'ldb-cl-macro-error "CL macro pre-expansion failed")

(defgroup ldb-cl-macro nil
  "Common Lisp macro pre-expansion for lisp-dialect-bridge."
  :group 'tools)

(defcustom ldb-cl-sbcl-program
  (or (executable-find "sbcl")
      (let ((p "/usr/bin/sbcl")) (and (file-executable-p p) p)))
  "Path to the SBCL executable used for CL macro pre-expansion.
When nil, `ldb-cl-macro-expand-string' signals `ldb-cl-macro-error'."
  :type '(choice (const :tag "Not available" nil) file)
  :group 'ldb-cl-macro)

(defconst ldb-cl-macro--walker "\
(defun user-macro-p (op)
  (and (symbolp op)
       (macro-function op)
       (not (eq (symbol-package op) (find-package :common-lisp)))))

(defun walk (form)
  (cond
    ((not (consp form)) form)
    ((eq (car form) 'quote) form)
    ((user-macro-p (car form)) (walk (macroexpand-1 form)))
    (t (cons (if (consp (car form)) (walk (car form)) (car form))
             (mapcar #'walk (cdr form))))))

(defun process (path)
  (let ((forms '()))
    (with-open-file (in path)
      (loop for f = (read in nil :eof) until (eq f :eof) do (push f forms)))
    (setf forms (nreverse forms))
    (dolist (f forms)
      (when (and (consp f)
                 (member (car f) '(defmacro defun defvar defparameter
                                   defconstant define-symbol-macro)))
        (handler-case (eval f)
          (error (e) (format *error-output* \"pass1: ~a~%\" e)))))
    (let ((out (with-output-to-string (s)
                 (let ((*print-case* :downcase) (*print-length* nil)
                       (*print-level* nil) (*print-circle* t)
                       (*print-pretty* nil) (*print-readably* nil))
                   (dolist (f forms)
                     (unless (and (consp f) (eq (car f) 'defmacro))
                       (prin1 (walk f) s) (terpri s)))))))
      (write-string out))))

(handler-case (process (second sb-ext:*posix-argv*))
  (error (e) (format *error-output* \"preexpand: ~a~%\" e)
    (sb-ext:exit :code 3)))
"
  "SBCL script: selective user-macro expander, emitting downcased CL text.
Reads the CL source path from the second element of `sb-ext:*posix-argv*'
and writes the macro-expanded (use-site) program to standard output.")

;;;###autoload
(defun ldb-cl-macro-preexpand-available-p ()
  "Return the SBCL program path when macro pre-expansion is possible, else nil."
  (and ldb-cl-sbcl-program (file-executable-p ldb-cl-sbcl-program)
       ldb-cl-sbcl-program))

;;;###autoload
(defun ldb-cl-macro-expand-string (source)
  "Pre-expand user macros in Common Lisp SOURCE (string) via SBCL.
Return the expanded CL source as a string (lowercased symbols, `defmacro'
definitions dropped, only standard CL / core forms remaining).  Signal
`ldb-cl-macro-error' when SBCL is unavailable or pre-expansion fails."
  (let ((sbcl (ldb-cl-macro-preexpand-available-p)))
    (unless sbcl
      (signal 'ldb-cl-macro-error
              (list "SBCL not available for macro pre-expansion")))
    (let ((walker (make-temp-file "ldb-walker-" nil ".lisp"))
          (src (make-temp-file "ldb-src-" nil ".lisp"))
          (errfile (make-temp-file "ldb-err-" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file walker (insert ldb-cl-macro--walker))
            (with-temp-file src (insert source))
            (with-temp-buffer
              (let ((code (call-process sbcl nil (list (current-buffer) errfile)
                                        nil "--script" walker src)))
                (unless (eq code 0)
                  (signal 'ldb-cl-macro-error
                          (list (format "SBCL pre-expansion failed (exit %s): %s"
                                        code
                                        (with-temp-buffer
                                          (insert-file-contents errfile)
                                          (string-trim (buffer-string)))))))
                (buffer-string))))
        (delete-file walker)
        (delete-file src)
        (delete-file errfile)))))

;;;###autoload
(defun ldb-cl-translate-string-expanding (source)
  "Translate Common Lisp SOURCE to Elisp forms, pre-expanding user macros.
Runs SOURCE through `ldb-cl-macro-expand-string' (SBCL) first, then the
normal `ldb-cl-translate-string'.  Use this instead of
`ldb-cl-translate-string' when SOURCE defines and uses `defmacro'."
  (ldb-cl-translate-string (ldb-cl-macro-expand-string source)))

;;;###autoload
(defun ldb-cl-translate-file-expanding (path)
  "Translate the Common Lisp file at PATH to Elisp forms, pre-expanding macros.
Like `ldb-cl-translate-file' but runs the file through SBCL macro
pre-expansion (`ldb-cl-macro-expand-string') first."
  (ldb-cl-translate-string-expanding
   (with-temp-buffer (insert-file-contents path) (buffer-string))))

(provide 'ldb-cl-macro)
;;; ldb-cl-macro.el ends here
