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

(provide 'ldb-emit-elisp)
;;; ldb-emit-elisp.el ends here
