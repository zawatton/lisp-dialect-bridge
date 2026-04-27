;;; lisp-dialect-bridge.el --- Lisp-family <-> Emacs Lisp interop layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton
;; Maintainer: zawatton
;; URL: https://github.com/zawatton/lisp-dialect-bridge
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, languages, lisp, scheme

;; This file is part of lisp-dialect-bridge.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Top-level loader.  Pulls in the Phase 1 modules so that
;; `(require 'lisp-dialect-bridge)' is enough to use
;; `ldb-guix-import-file' / `ldb-guix-import-string' /
;; `ldb-guix-import-form'.
;;
;; Module map:
;;   ldb-ir.el            — IR node primitives
;;   ldb-scheme.el        — Scheme reader (preprocess + Emacs read)
;;   ldb-emit-elisp.el    — IR -> pkg-define form emitter
;;   ldb-guix-importer.el — public entry: Guix recipe -> pkg-define
;;
;; Design doc: docs/design/01-overview.org.

;;; Code:

(require 'ldb-ir)
(require 'ldb-scheme)
(require 'ldb-emit-elisp)
(require 'ldb-guix-importer)

(provide 'lisp-dialect-bridge)
;;; lisp-dialect-bridge.el ends here
