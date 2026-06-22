;;; ldb-mcp.el --- Optional anvil MCP adapter for lisp-dialect-bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; OPTIONAL integration: expose the bridge's translators as anvil MCP tools
;; on the "emacs-eval" server, so an agent (Claude Code) can call them as
;; `mcp__emacs-eval__ldb-translate' etc. instead of shelling out to
;; `emacs --batch'.  This is the ONLY file in the bridge that requires
;; anvil; the core translators (`ldb-cl', `ldb-scheme-cvt', ...) stay
;; anvil-free (CLAUDE.md invariant 5).  It is therefore NOT part of the
;; `make compile' SRC list — it is loaded only inside a running anvil
;; daemon, which already provides `anvil-server'.
;;
;; Enable (in the daemon):
;;   (add-to-list 'load-path "/path/to/lisp-dialect-bridge")
;;   (require 'lisp-dialect-bridge)   ; or the individual ldb-* modules
;;   (load "/path/to/lisp-dialect-bridge/ldb-mcp.el")
;;   (ldb-mcp-enable)
;;
;; Tools:
;;   ldb-translate         dialect + source/path [+ expand + pretty] -> Elisp
;;   ldb-translate-to-file dialect + out + source/path [+ expand]     -> .el file
;;   ldb-capabilities      (no args) -> dialects / oracles / rejects (live)

;;; Code:

(require 'anvil-server)
(require 'ldb-cl)
(require 'ldb-cl-macro)
(require 'ldb-scheme-cvt)
(require 'ldb-scheme-macro)

(defvar ldb-mcp--server-id "emacs-eval"
  "MCP server id to register the bridge tools on (matches anvil-file).")

;;;; --- helpers --------------------------------------------------------------

(defun ldb-mcp--truthy (s)
  "Non-nil when string flag S means on (\"1\"/\"t\"/\"true\"/\"yes\")."
  (and s (stringp s) (member (downcase s) '("1" "t" "true" "yes" "on"))))

(defun ldb-mcp--source-of (source path)
  "Return the source string from SOURCE or by reading PATH; error if neither."
  (cond
   ((and source (stringp source) (not (string-empty-p source))) source)
   ((and path (stringp path) (not (string-empty-p path)))
    (with-temp-buffer (insert-file-contents path) (buffer-string)))
   (t (error "Provide `source' or `path'"))))

(defun ldb-mcp--translate (dialect code expand)
  "Translate CODE in DIALECT (cl/scheme); EXPAND non-nil pre-expands macros."
  (cond
   ((member dialect '("cl" "common-lisp" "commonlisp" "lisp"))
    (if expand (ldb-cl-translate-string-expanding code)
      (ldb-cl-translate-string code)))
   ((member dialect '("scheme" "scm" "r7rs" "guile"))
    (if expand (ldb-scheme-translate-string-expanding code)
      (ldb-scheme-translate-string code)))
   (t (error "Unknown dialect %S (use \"cl\" or \"scheme\")" dialect))))

(defun ldb-mcp--render (forms pretty)
  "Render translated FORMS as Elisp text; PRETTY non-nil pretty-prints."
  (mapconcat (lambda (f) (string-trim-right
                          (if pretty (pp-to-string f) (prin1-to-string f))))
             forms "\n"))

(defun ldb-mcp--prog-version (name)
  "Return \"PATH (VERSION)\" for external program NAME, or :absent."
  (let ((p (executable-find name)))
    (if (not p) :absent
      (let ((line (with-temp-buffer
                    (ignore-errors (call-process name nil t nil "--version"))
                    (car (split-string (buffer-string) "\n" t)))))
        (format "%s%s" p (if line (format " (%s)" (string-trim line)) ""))))))

;;;; --- tool handlers --------------------------------------------------------

(defun ldb-mcp--tool-translate (dialect &optional source path expand pretty)
  "Translate Common Lisp or Scheme SOURCE to Emacs Lisp (returned as text).

MCP Parameters:
  dialect - \"cl\" (Common Lisp) or \"scheme\"
  source - source code string (provide this OR path)
  path - absolute path to a source file (alternative to source)
  expand - \"1\" to pre-expand macros first (SBCL for cl / Guile for scheme)
  pretty - \"1\" (default) to pretty-print output; \"0\" for compact"
  (anvil-server-with-error-handling
    (let* ((code (ldb-mcp--source-of source path))
           (forms (ldb-mcp--translate dialect code (ldb-mcp--truthy expand)))
           (pp (not (member pretty '("0" "" "nil" "false" "no")))))
      (ldb-mcp--render forms pp))))

(defun ldb-mcp--tool-translate-to-file (dialect out &optional source path expand)
  "Translate Common Lisp or Scheme source and WRITE the Elisp to OUT (.el).

MCP Parameters:
  dialect - \"cl\" (Common Lisp) or \"scheme\"
  out - absolute path of the .el file to write
  source - source code string (provide this OR path)
  path - absolute path to a source file (alternative to source)
  expand - \"1\" to pre-expand macros first (SBCL for cl / Guile for scheme)"
  (anvil-server-with-error-handling
    (let* ((code (ldb-mcp--source-of source path))
           (forms (ldb-mcp--translate dialect code (ldb-mcp--truthy expand))))
      (with-temp-file out
        (insert ";;; -*- lexical-binding: t; -*-\n"
                (format ";;; Translated from %s by lisp-dialect-bridge.\n\n" dialect))
        (dolist (f forms) (insert (pp-to-string f) "\n")))
      (format "%S" (list :file out :forms (length forms))))))

(defun ldb-mcp--tool-capabilities ()
  "Report the bridge's live capabilities: dialects, oracles, reject lists.

MCP Parameters:"
  (anvil-server-with-error-handling
    (format "%S"
            (list
             :dialects '("cl" "scheme")
             :oracles (list :sbcl (ldb-mcp--prog-version "sbcl")
                            :guile (ldb-mcp--prog-version "guile"))
             :cl (list :macros "defmacro via SBCL pre-expand (expand=1)"
                       :rejected (and (boundp 'ldb-cl--reject-heads)
                                      ldb-cl--reject-heads))
             :scheme (list :macros "define-syntax via Guile pre-expand (expand=1)"
                           :rejected (and (boundp 'ldb-scheme--reject-heads)
                                          ldb-scheme--reject-heads)
                           :notes '("#t/#f -> t/nil"
                                    "()-as-true is a documented gap"
                                    "no TCO guarantee (deep tail recursion may overflow)"))))))

;;;; --- registration ---------------------------------------------------------

;;;###autoload
(defun ldb-mcp-enable ()
  "Register the lisp-dialect-bridge MCP tools on the emacs-eval server."
  (anvil-server-register-tool
   #'ldb-mcp--tool-translate
   :id "ldb-translate"
   :read-only t
   :server-id ldb-mcp--server-id
   :description
   "Translate Common Lisp or Scheme source code to Emacs Lisp via
lisp-dialect-bridge.  Pass dialect \"cl\" or \"scheme\" and either an
inline `source' string or a `path' to a file.  Set expand=\"1\" to
pre-expand macros first (defmacro via SBCL for cl, define-syntax via
Guile for scheme).  Returns the translated Elisp forms as text.")
  (anvil-server-register-tool
   #'ldb-mcp--tool-translate-to-file
   :id "ldb-translate-to-file"
   :server-id ldb-mcp--server-id
   :description
   "Translate a Common Lisp or Scheme source (string or file) and write the
resulting Emacs Lisp to the `out' .el path.  Same dialect/expand options
as ldb-translate.  Use this to import a CL/Scheme library file into .el.")
  (anvil-server-register-tool
   #'ldb-mcp--tool-capabilities
   :id "ldb-capabilities"
   :read-only t
   :server-id ldb-mcp--server-id
   :description
   "Report the bridge's current capabilities (no arguments): supported
dialects, whether the SBCL/Guile macro oracles are present, and the live
reject lists per dialect.  Call before translating to know if a form is
in scope or needs expand=1.")
  (list :enabled '("ldb-translate" "ldb-translate-to-file" "ldb-capabilities")
        :server ldb-mcp--server-id))

;;;###autoload
(defun ldb-mcp-disable ()
  "Unregister the lisp-dialect-bridge MCP tools."
  (dolist (id '("ldb-translate" "ldb-translate-to-file" "ldb-capabilities"))
    (ignore-errors (anvil-server-unregister-tool id ldb-mcp--server-id))))

(provide 'ldb-mcp)
;;; ldb-mcp.el ends here
