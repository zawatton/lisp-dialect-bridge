;;; ldb-scheme-sicp-test.el --- Real Scheme (SICP) via translate+run -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5 validation on REAL, canonical Scheme: verbatim procedures from
;; SICP (Abelson & Sussman).  The Scheme analogue of the alexandria CL
;; validation.  Each program is translated to Elisp, run here, and diffed
;; against GNU Guile running the same source (the oracle + macro come from
;; `ldb-scheme-guile-diff').  Folded into `make schemediff'.
;;
;; The set stresses exactly where Scheme is hard (Lisp-1):
;;   * higher-order `sum' / `accumulate' (procedure PARAMETERS called as
;;     functions -> funcall; named procedures passed by value -> #'),
;;   * tree recursion (`count-change'), Newton's-method `sqrt',
;;   * symbolic differentiation `deriv' (SICP 2.3.2) -- the classic
;;     GOFAI program, the Scheme twin of the CL PAIP validation: builds
;;     and simplifies symbolic expressions, returns lists/symbols.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ldb-scheme-cvt)
(require 'ldb-scheme-guile-diff)

;;;; --- verbatim SICP sources ------------------------------------------------

;; SICP 1.1.7 — square roots by Newton's method.
(defconst ldb-scheme-sicp--sqrt "\
(define (my-sqrt x) (sqrt-iter 1.0 x))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (improve guess x) (average guess (/ x guess)))
(define (average a b) (/ (+ a b) 2))
(define (good-enough? guess x) (< (abs (- (* guess guess) x)) 0.0001))")

;; SICP 1.3.1 — higher-order `sum' (term and next are PROCEDURE params).
(defconst ldb-scheme-sicp--sum "\
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
(define (cube x) (* x x x))
(define (inc n) (+ n 1))")

;; SICP 1.2.2 — counting change (tree recursion).
(defconst ldb-scheme-sicp--count-change "\
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))")

;; SICP 2.2.3 — accumulate (a fold; `op' is a procedure param, `+'/`*'
;; passed by value).
(defconst ldb-scheme-sicp--accumulate "\
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))")

;; SICP 2.3.2 — symbolic differentiation (the GOFAI classic).
(defconst ldb-scheme-sicp--deriv "\
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else (error \"unknown expression type\"))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))")

;;;; --- differential cases (vs Guile) ----------------------------------------

;; Newton's sqrt: compare via tolerance to dodge float-print differences.
(ldb-scheme-guile-diff-deftest ldb-scheme-sicp/sqrt
  ldb-scheme-sicp--sqrt
  "(< (abs (- (my-sqrt 16) 4)) 0.01)")

(ldb-scheme-guile-diff-deftest ldb-scheme-sicp/sum-cubes
  ldb-scheme-sicp--sum
  "(sum cube 1 inc 10)")

(ldb-scheme-guile-diff-deftest ldb-scheme-sicp/sum-integers
  ldb-scheme-sicp--sum
  "(sum (lambda (x) x) 1 inc 100)")

(ldb-scheme-guile-diff-deftest ldb-scheme-sicp/count-change
  ldb-scheme-sicp--count-change
  "(count-change 100)")

(ldb-scheme-guile-diff-deftest ldb-scheme-sicp/accumulate-sum
  ldb-scheme-sicp--accumulate
  "(accumulate + 0 (list 1 2 3 4 5))")

(ldb-scheme-guile-diff-deftest ldb-scheme-sicp/accumulate-product
  ldb-scheme-sicp--accumulate
  "(accumulate * 1 (list 1 2 3 4 5))")

(ldb-scheme-guile-diff-deftest ldb-scheme-sicp/deriv-sum
  ldb-scheme-sicp--deriv
  "(deriv '(+ x 3) 'x)")

(ldb-scheme-guile-diff-deftest ldb-scheme-sicp/deriv-product
  ldb-scheme-sicp--deriv
  "(deriv '(* x y) 'x)")

(ldb-scheme-guile-diff-deftest ldb-scheme-sicp/deriv-nested
  ldb-scheme-sicp--deriv
  "(deriv '(* (* x y) (+ x 3)) 'x)")

(provide 'ldb-scheme-sicp-test)
;;; ldb-scheme-sicp-test.el ends here
