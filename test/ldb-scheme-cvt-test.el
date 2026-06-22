;;; ldb-scheme-cvt-test.el --- Hermetic tests for Scheme core translation -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Hermetic ERT for `ldb-scheme-cvt' (no external Scheme needed): golden
;; shape (esp. the Lisp-1 -> Lisp-2 funcall / #' insertion), behavioural
;; eval, and loud rejection of out-of-scope forms.  Differential checks vs
;; Guile live in `ldb-scheme-guile-diff.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ldb-scheme-cvt)

(defun ldb-scheme-cvt-test--eval (src)
  "Translate Scheme SRC, eval every emitted form, return the last value."
  (let ((val nil))
    (dolist (f (ldb-scheme-translate-string src) val)
      (setq val (eval f t)))))

;;;; --- golden (Lisp-1 -> Lisp-2) --------------------------------------------

(ert-deftest ldb-scheme-cvt-test-define-golden ()
  "(define (f a b) ...) -> defun; a global call stays a direct call."
  (should (equal '(defun add3 (a b c) (+ a b c))
                 (car (ldb-scheme-translate-string "(define (add3 a b c) (+ a b c))")))))

(ert-deftest ldb-scheme-cvt-test-funcall-insertion ()
  "A locally-bound param called in head position becomes funcall."
  (should (equal '(defun apply-twice (f x) (funcall f (funcall f x)))
                 (car (ldb-scheme-translate-string
                       "(define (apply-twice f x) (f (f x)))")))))

(ert-deftest ldb-scheme-cvt-test-function-value-position ()
  "A global function passed by value becomes #'fn; a local stays bare."
  (should (equal '(progn
                    (defun sq (x) (* x x))
                    (cl-mapcar #'sq (list 1 2 3)))
                 (cons 'progn (ldb-scheme-translate-string
                               "(define (sq x) (* x x)) (map sq (list 1 2 3))")))))

(ert-deftest ldb-scheme-cvt-test-named-let-golden ()
  "Named let -> named-let; the loop name stays a direct call."
  (should (equal '(named-let loop ((i 0)) (if (= i 3) i (loop (+ i 1))))
                 (car (ldb-scheme-translate-string
                       "(let loop ((i 0)) (if (= i 3) i (loop (+ i 1))))")))))

(ert-deftest ldb-scheme-cvt-test-letrec-golden ()
  "letrec -> letrec; mutually-recursive locals are funcall'd."
  (should (equal '(letrec ((f (lambda (n) (funcall g n))) (g (lambda (n) n)))
                    (funcall f 1))
                 (car (ldb-scheme-translate-string
                       "(letrec ((f (lambda (n) (g n))) (g (lambda (n) n))) (f 1))")))))

(ert-deftest ldb-scheme-cvt-test-variadic-define ()
  "Dotted / bare params -> &rest."
  (should (equal '(defun f (a &rest rest) rest)
                 (car (ldb-scheme-translate-string "(define (f a . rest) rest)"))))
  (should (equal '(defun g (&rest args) args)
                 (car (ldb-scheme-translate-string "(define (g . args) args)")))))

;;;; --- behavioural ----------------------------------------------------------

(ert-deftest ldb-scheme-cvt-test-eval-recursion ()
  (should (= 120 (ldb-scheme-cvt-test--eval
                  "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)"))))

(ert-deftest ldb-scheme-cvt-test-eval-hof ()
  (should (= 81 (ldb-scheme-cvt-test--eval
                 "(define (apply-twice f x) (f (f x))) (apply-twice (lambda (y) (* y y)) 3)"))))

(ert-deftest ldb-scheme-cvt-test-eval-named-let ()
  (should (= 10 (ldb-scheme-cvt-test--eval
                 "(let loop ((i 0) (acc 0)) (if (= i 5) acc (loop (+ i 1) (+ acc i))))"))))

(ert-deftest ldb-scheme-cvt-test-eval-closure ()
  (should (= 15 (ldb-scheme-cvt-test--eval
                 "(define (adder n) (lambda (x) (+ x n))) ((adder 5) 10)"))))

(ert-deftest ldb-scheme-cvt-test-eval-cond-else ()
  (should (eq 'zero (ldb-scheme-cvt-test--eval
                     "(define (sign x) (cond ((< x 0) 'neg) ((> x 0) 'pos) (else 'zero))) (sign 0)"))))

;;;; --- loud rejection -------------------------------------------------------

(ert-deftest ldb-scheme-cvt-test-reject-define-syntax ()
  (should-error (ldb-scheme-translate-string "(define-syntax swap! (syntax-rules () ((_ a b) #t)))")
                :type 'ldb-scheme-unsupported-form-error))

(ert-deftest ldb-scheme-cvt-test-reject-callcc ()
  (should-error (ldb-scheme-translate-string "(call/cc (lambda (k) (k 1)))")
                :type 'ldb-scheme-unsupported-form-error))

(ert-deftest ldb-scheme-cvt-test-reject-values ()
  (should-error (ldb-scheme-translate-string "(values 1 2)")
                :type 'ldb-scheme-unsupported-form-error))

(ert-deftest ldb-scheme-cvt-test-quasiquote-golden ()
  "Scheme quasiquote -> Elisp backquote; unquote/splice round-trip."
  (should (equal '`(a ,b ,@c)
                 (car (ldb-scheme-translate-string "`(a ,b ,@c)")))))

(ert-deftest ldb-scheme-cvt-test-quasiquote-eval ()
  "Unquoted exprs are env/funcall-aware and splice correctly."
  (should (equal '(a 5 1 2)
                 (ldb-scheme-cvt-test--eval
                  "(let ((b 5) (c (list 1 2))) `(a ,b ,@c))")))
  (should (equal '(sq 9)
                 (ldb-scheme-cvt-test--eval "(let ((x 3)) `(sq ,(* x x)))"))))

(ert-deftest ldb-scheme-cvt-test-reject-nested-quasiquote ()
  (should-error (ldb-scheme-translate-string "`(a `(b ,c))")
                :type 'ldb-scheme-unsupported-form-error))

(ert-deftest ldb-scheme-cvt-test-reject-stray-unquote ()
  (should-error (ldb-scheme-translate-string "(+ 1 ,x)")
                :type 'ldb-scheme-unsupported-form-error))

(provide 'ldb-scheme-cvt-test)
;;; ldb-scheme-cvt-test.el ends here
