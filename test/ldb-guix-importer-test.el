;;; ldb-guix-importer-test.el --- ERT for the Phase 1 Guix importer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; This file is part of lisp-dialect-bridge.  GPL-3.0-or-later.

;;; Commentary:

;; Phase 1 ERT coverage.  Mocked input is one or two literal Guix
;; recipes embedded as strings; no Scheme implementation needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lisp-dialect-bridge)

;;;; --- Scheme reader -------------------------------------------------------

(ert-deftest ldb-test-scheme-read-package-form ()
  "Scheme preprocessing + Emacs read recover Guix `package' shape."
  (let* ((src "(define-public hello
                 (package
                   (name \"hello\")
                   (version \"2.12.1\")
                   (source (origin
                             (method url-fetch)
                             (uri \"https://ftp.gnu.org/gnu/hello/hello-2.12.1.tar.gz\")
                             (sha256 (base32 \"abc123\"))))
                   (build-system stdenv-build-system)
                   (synopsis \"Hello, world example program\")
                   (home-page \"https://www.gnu.org/software/hello/\")
                   (license license:gpl3+)))")
         (forms (ldb-scheme-read-all-from-string src)))
    (should (= 1 (length forms)))
    (let ((top (car forms)))
      (should (eq 'define-public (car top)))
      (should (eq 'hello (cadr top)))
      (should (eq 'package (car (caddr top)))))))

(ert-deftest ldb-test-scheme-preprocess-substitutions ()
  "Preprocessor maps #t/#f/#:keyword to Elisp-readable tokens."
  (let* ((src "(foo #t #f #:bar)")
         (form (ldb-scheme-read-from-string src)))
    (should (equal '(foo t nil :bar) form))))

;;;; --- Phase 1 reject list -------------------------------------------------

(ert-deftest ldb-test-reject-quasiquote-inputs ()
  "Quasiquote in inputs raises ldb-unsupported-guix-form-error."
  (let ((src "(define-public x
                (package
                  (name \"x\")
                  (version \"1\")
                  (source (origin (method url-fetch)
                                  (uri \"u\")
                                  (sha256 (base32 \"h\"))))
                  (build-system stdenv-build-system)
                  (inputs `((\"a\" ,a)))))"))
    (should-error (ldb-guix-import-string src 'x)
                  :type 'ldb-unsupported-guix-form-error)))

(ert-deftest ldb-test-reject-missing-define-public ()
  "Missing (define-public NAME ...) raises."
  (let ((src "(some-other-form)"))
    (should-error (ldb-guix-import-string src 'no-such)
                  :type 'ldb-unsupported-guix-form-error)))

;;;; --- Full pipeline goldens ----------------------------------------------

(ert-deftest ldb-test-import-stdenv-url-fetch-golden ()
  "End-to-end: GNU Hello recipe -> pkg-define golden form."
  (let* ((src "(define-public hello
                 (package
                   (name \"hello\")
                   (version \"2.12.1\")
                   (source (origin
                             (method url-fetch)
                             (uri \"https://ftp.gnu.org/gnu/hello/hello-2.12.1.tar.gz\")
                             (sha256 (base32 \"abc123\"))))
                   (build-system stdenv-build-system)
                   (synopsis \"Hello, world example program\")
                   (home-page \"https://www.gnu.org/software/hello/\")
                   (license license:gpl3+)))")
         (out (ldb-guix-import-string src 'hello))
         (expected
          '(pkg-define hello
             (version "2.12.1")
             (source (url-fetch "https://ftp.gnu.org/gnu/hello/hello-2.12.1.tar.gz"
                                :sha256 "abc123"))
             (build-system stdenv)
             (description "Hello, world example program")
             (homepage "https://www.gnu.org/software/hello/")
             (license gpl3))))
    (should (equal expected out))))

(ert-deftest ldb-test-import-cargo-build-system ()
  "cargo-build-system maps to (build-system (rust)) — :cargo-sha256 left for user."
  (let* ((src "(define-public ripgrep
                 (package
                   (name \"ripgrep\")
                   (version \"13.0.0\")
                   (source (origin
                             (method url-fetch)
                             (uri \"https://example.com/rg-13.0.0.tar.gz\")
                             (sha256 (base32 \"sha256-rg\"))))
                   (build-system cargo-build-system)
                   (license license:expat)))")
         (out (ldb-guix-import-string src 'ripgrep)))
    (should (equal '(build-system (rust))
                   (assq 'build-system out)))
    (should (equal '(license mit)
                   (assq 'license out)))))

(ert-deftest ldb-test-import-python-build-system ()
  "python-build-system maps to (build-system python)."
  (let* ((src "(define-public python-requests
                 (package
                   (name \"python-requests\")
                   (version \"2.31.0\")
                   (source (origin
                             (method url-fetch)
                             (uri \"https://example.com/requests-2.31.0.tar.gz\")
                             (sha256 (base32 \"sha256-req\"))))
                   (build-system python-build-system)
                   (license license:asl2.0)))")
         (out (ldb-guix-import-string src 'python-requests)))
    (should (equal '(build-system python)
                   (assq 'build-system out)))
    (should (equal '(license apache2)
                   (assq 'license out)))))

(ert-deftest ldb-test-import-git-fetch ()
  "(method git-fetch) + (git-reference ...) maps to (git-fetch :url ... :rev ...)."
  (let* ((src "(define-public somerepo
                 (package
                   (name \"somerepo\")
                   (version \"0.1.0\")
                   (source (origin
                             (method git-fetch)
                             (uri (git-reference
                                    (url \"https://example.com/somerepo.git\")
                                    (commit \"abc1234\")))
                             (sha256 (base32 \"sha256-git\"))))
                   (build-system stdenv-build-system)))")
         (out (ldb-guix-import-string src 'somerepo))
         (src-form (cadr (assq 'source out))))
    (should (eq 'git-fetch (car src-form)))
    (should (equal '(:url "https://example.com/somerepo.git"
                     :rev "abc1234"
                     :sha256 "sha256-git")
                   (cdr src-form)))))

(ert-deftest ldb-test-import-inputs-list ()
  "(inputs (list a b c)) maps verbatim to (list a b c)."
  (let* ((src "(define-public x
                 (package
                   (name \"x\") (version \"1\")
                   (source (origin (method url-fetch) (uri \"u\")
                                   (sha256 (base32 \"h\"))))
                   (build-system stdenv-build-system)
                   (inputs (list pkg-config openssl))))")
         (out (ldb-guix-import-string src 'x)))
    (should (equal '(inputs (list pkg-config openssl))
                   (assq 'inputs out)))))

(provide 'ldb-guix-importer-test)
;;; ldb-guix-importer-test.el ends here
