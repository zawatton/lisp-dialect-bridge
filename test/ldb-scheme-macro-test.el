;;; ldb-scheme-macro-test.el --- Tests for Scheme define-syntax pre-expansion -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5: exercises `ldb-scheme-macro' (Guile `tree-il->scheme'
;; pre-expansion of `define-syntax').  Differential: run the original
;; macro program in Guile (which handles define-syntax natively) and the
;; (pre-expand -> translate -> eval) path here, assert agreement.  Reuses
;; the Guile oracle helpers from `ldb-scheme-guile-diff'.  Gated on Guile.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ldb-scheme-cvt)
(require 'ldb-scheme-macro)
(require 'ldb-scheme-guile-diff)

(defun ldb-scheme-macro-test--bridge (source probe)
  "Pre-expand SOURCE+PROBE via Guile, translate, eval, return printed value."
  (let* ((expanded (ldb-scheme-macro-expand-string (concat source "\n" probe)))
         (all (ldb-scheme-translate-string expanded))
         (defs (butlast all))
         (tail (car (last all))))
    (dolist (f defs) (eval f t))
    (prin1-to-string (eval tail t))))

(defmacro ldb-scheme-macro-test-difftest (name source probe)
  "Define ERT test NAME: Guile on original vs bridge pre-expand+translate+eval."
  (declare (indent 1))
  `(ert-deftest ,name ()
     (skip-unless (ldb-scheme-macro-preexpand-available-p))
     (should (equal (ldb-scheme-guile-diff--normalize
                     (ldb-scheme-guile-diff--guile-eval ,source ,probe))
                    (ldb-scheme-guile-diff--normalize
                     (ldb-scheme-macro-test--bridge ,source ,probe))))))

;;;; --- unit: availability / shape -------------------------------------------

(ert-deftest ldb-scheme-macro/availability ()
  (skip-unless (ldb-scheme-macro-preexpand-available-p))
  (should (file-executable-p (ldb-scheme-macro-preexpand-available-p))))

(ert-deftest ldb-scheme-macro/expand-drops-define-syntax ()
  (skip-unless (ldb-scheme-macro-preexpand-available-p))
  (let ((out (ldb-scheme-macro-expand-string
              "(define-syntax inc! (syntax-rules () ((_ x) (set! x (+ x 1)))))\n(let ((c 0)) (inc! c) c)")))
    (should-not (string-match-p "define-syntax" out))
    (should (string-match-p "set!" out))))

(ert-deftest ldb-scheme-macro/expand-no-guile-signals ()
  (let ((ldb-scheme-guile-program nil))
    (should-error (ldb-scheme-macro-expand-string "(define x 1)")
                  :type 'ldb-scheme-macro-error)))

;;;; --- differential: Guile oracle vs pre-expanded bridge --------------------

(defconst ldb-scheme-macro--swap
  "(define-syntax swap! (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))")

(ldb-scheme-macro-test-difftest ldb-scheme-macro/swap
  ldb-scheme-macro--swap
  "(let ((x 1) (y 2)) (swap! x y) (list x y))")

;; Hygiene end-to-end: the caller's var is literally named `tmp'; the
;; macro's `tmp' must NOT capture it.  (9 5), not something broken.
(ldb-scheme-macro-test-difftest ldb-scheme-macro/swap-hygiene
  ldb-scheme-macro--swap
  "(let ((tmp 5) (y 9)) (swap! tmp y) (list tmp y))")

(ldb-scheme-macro-test-difftest ldb-scheme-macro/my-when-true
  "(define-syntax my-when (syntax-rules () ((_ c e ...) (if c (begin e ...) #f))))"
  "(my-when (> 3 0) 10 20 30)")

(ldb-scheme-macro-test-difftest ldb-scheme-macro/my-unless
  "(define-syntax my-unless (syntax-rules () ((_ c e ...) (if c #f (begin e ...)))))"
  "(my-unless (> 0 3) 'ran)")

(ldb-scheme-macro-test-difftest ldb-scheme-macro/ellipsis-list
  "(define-syntax my-list (syntax-rules () ((_ e ...) (list e ...))))"
  "(my-list 1 2 (+ 1 2) 4)")

(ldb-scheme-macro-test-difftest ldb-scheme-macro/inc-in-loop
  "(define-syntax inc! (syntax-rules () ((_ x) (set! x (+ x 1)))))"
  "(let loop ((i 0) (c 0)) (if (= i 5) c (begin (inc! c) (loop (+ i 1) c))))")

;; A classic `while' macro: expands to named-let + when (both in subset).
(ldb-scheme-macro-test-difftest ldb-scheme-macro/while-loop
  "(define-syntax while
     (syntax-rules ()
       ((_ cond body ...) (let loop () (when cond body ... (loop))))))"
  "(let ((i 0) (s 0)) (while (< i 5) (set! s (+ s i)) (set! i (+ i 1))) s)")

;; A macro that generates a definition.
(ldb-scheme-macro-test-difftest ldb-scheme-macro/macro-defines
  "(define-syntax def-double
     (syntax-rules () ((_ name x) (define (name) (* 2 x)))))
   (def-double twelve 6)"
  "(twelve)")

(provide 'ldb-scheme-macro-test)
;;; ldb-scheme-macro-test.el ends here
