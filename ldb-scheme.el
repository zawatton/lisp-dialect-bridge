;;; ldb-scheme.el --- Minimal Scheme reader for Phase 1 Guix recipes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; This file is part of lisp-dialect-bridge.  GPL-3.0-or-later.

;;; Commentary:

;; Reads Guix-flavoured Scheme source into Elisp S-expressions.  The
;; approach is /pre-process + Emacs read/, not a custom parser:
;; substitute the few Scheme tokens that Elisp's reader does not know,
;; then call `read'.  This keeps Phase 1 minimal.
;;
;; Phase 1 substitutions:
;;   #t / #true              -> t
;;   #f / #false             -> nil
;;   #:keyword               -> :keyword
;;   #;<datum>               -> (stripped — datum comment)
;;   #| block comment |#     -> (stripped)
;;
;; Phase 1 limitations (will signal a downstream parse / convert error):
;;   #(vector)                       — not handled
;;   #\char                           — not handled
;;   #~ / #$  (Guix gexps)           — appear inside arguments / phases
;;                                     fields which Phase 1 rejects anyway
;;   `(...)  / ,X / ,@X (quasiquote)  — surfaces as Elisp's (\` ...) form;
;;                                     the IR converter rejects loudly

;;; Code:

(define-error 'ldb-scheme-read-error "Scheme read error")

(defun ldb-scheme-read-from-string (str)
  "Read the first Scheme expression from STR.  Returns Elisp S-expression."
  (let ((pre (ldb-scheme--preprocess str)))
    (with-temp-buffer
      (insert pre)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun ldb-scheme-read-all-from-string (str)
  "Read every top-level Scheme expression from STR.  Returns list."
  (let ((pre (ldb-scheme--preprocess str)))
    (with-temp-buffer
      (insert pre)
      (goto-char (point-min))
      (let (acc)
        (condition-case _
            (while t
              (push (read (current-buffer)) acc))
          (end-of-file nil)
          (invalid-read-syntax nil))
        (nreverse acc)))))

(defun ldb-scheme-read-file (path)
  "Read every top-level Scheme expression from file at PATH."
  (ldb-scheme-read-all-from-string
   (with-temp-buffer
     (insert-file-contents path)
     (buffer-string))))

(defun ldb-scheme--preprocess (str)
  "Convert Scheme-specific surface tokens in STR to Elisp-readable form.
See Commentary for the substitution table."
  (with-temp-buffer
    (insert str)
    ;; Block comments first, before any other rule sees the tokens inside.
    (goto-char (point-min))
    (while (search-forward "#|" nil t)
      (let ((start (- (point) 2)))
        (when (search-forward "|#" nil t)
          (delete-region start (point)))))
    ;; Datum comments: #;<sexp> deletes the next datum.
    (goto-char (point-min))
    (while (search-forward "#;" nil t)
      (let ((start (- (point) 2)))
        (skip-chars-forward " \t\n")
        (cond
         ((eq (char-after) ?\()
          (forward-sexp))
         (t
          (skip-chars-forward "^ \t\n()")))
        (delete-region start (point))))
    ;; #true / #false (longer first, word-bounded).
    (goto-char (point-min))
    (while (re-search-forward "#true\\>"  nil t) (replace-match "t"   t t))
    (goto-char (point-min))
    (while (re-search-forward "#false\\>" nil t) (replace-match "nil" t t))
    ;; #t / #f
    (goto-char (point-min))
    (while (re-search-forward "#t\\>" nil t) (replace-match "t"   t t))
    (goto-char (point-min))
    (while (re-search-forward "#f\\>" nil t) (replace-match "nil" t t))
    ;; #:keyword
    (goto-char (point-min))
    (while (re-search-forward "#:\\([A-Za-z][A-Za-z0-9_+:-]*\\)" nil t)
      (replace-match ":\\1"))
    (buffer-string)))

(provide 'ldb-scheme)
;;; ldb-scheme.el ends here
