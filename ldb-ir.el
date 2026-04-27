;;; ldb-ir.el --- IR node definitions for lisp-dialect-bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; This file is part of lisp-dialect-bridge.  GPL-3.0-or-later.

;;; Commentary:

;; Shared intermediate representation for Lisp-family dialect translation.
;;
;; A node is a plist:
;;   (:tag SYMBOL :form ANY :origin SYMBOL :meta PLIST)
;;
;; The shape is frozen after Phase 0 (design doc 01-overview.org L2).
;; New per-dialect information goes in :form or :meta — never new
;; top-level keys.

;;; Code:

(defun ldb-ir-make (tag &rest plist)
  "Build an IR node with TAG and PLIST kvs.

PLIST may contain :form, :origin, :meta — the standard sub-keys."
  (apply #'list :tag tag plist))

(defun ldb-ir-tag (node)
  "Return the IR tag of NODE."
  (plist-get node :tag))

(defun ldb-ir-form (node)
  "Return the :form payload of NODE."
  (plist-get node :form))

(defun ldb-ir-origin (node)
  "Return the source dialect of NODE (`scheme', `cl', `nix', `elisp')."
  (plist-get node :origin))

(defun ldb-ir-meta (node)
  "Return the :meta plist of NODE."
  (plist-get node :meta))

;;;; --- predicates -----------------------------------------------------------

(defun ldb-ir-literal-p (node)  (eq 'literal (ldb-ir-tag node)))
(defun ldb-ir-ref-p     (node)  (eq 'ref     (ldb-ir-tag node)))
(defun ldb-ir-record-p  (node)  (eq 'record  (ldb-ir-tag node)))
(defun ldb-ir-list-of-p (node)  (eq 'list-of (ldb-ir-tag node)))

;;;; --- record helpers --------------------------------------------------------

(defun ldb-ir-record-type (node)
  "For a record NODE, return its :type symbol (e.g. `package', `origin')."
  (plist-get (ldb-ir-form node) :type))

(defun ldb-ir-record-fields (node)
  "For a record NODE, return its alist `((FIELD-NAME . IR-NODE) ...)'.

Field tuples are stored as 2-element lists, so callers should use
`ldb-ir-record-field' / `assq' on the list."
  (plist-get (ldb-ir-form node) :fields))

(defun ldb-ir-record-field (node name)
  "Return the IR node stored under field NAME in record NODE, or nil."
  (cadr (assq name (ldb-ir-record-fields node))))

;;;; --- literal helpers -------------------------------------------------------

(defun ldb-ir-literal-value (node)
  "Return the underlying value of a literal NODE."
  (plist-get (ldb-ir-form node) :value))

(defun ldb-ir-ref-name (node)
  "Return the symbol name of a ref NODE."
  (plist-get (ldb-ir-form node) :name))

(provide 'ldb-ir)
;;; ldb-ir.el ends here
