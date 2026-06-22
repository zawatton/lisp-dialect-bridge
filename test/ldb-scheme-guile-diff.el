;;; ldb-scheme-guile-diff.el --- Differential tests: Guile oracle vs bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The Scheme twin of `ldb-cl-sbcl-diff': GNU Guile 3.0 is the ground-truth
;; oracle.  Each case runs one Scheme program two ways — Guile `-s' on the
;; source, and the bridge translating the same source to Elisp and this
;; Emacs running it — and asserts the printed values agree after
;; normalisation.  Normalisation canonicalises the dialect surface
;; differences: Scheme `#t'/`#f'/`()' vs Elisp `t'/`nil'/`nil' (+ trim,
;; downcase).  Skips when guile is absent (`skip-unless').

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ldb-scheme-cvt)

(defvar ldb-scheme-guile-program
  (or (executable-find "guile")
      (let ((p "/usr/bin/guile")) (and (file-executable-p p) p)))
  "Path to GNU Guile, or nil when unavailable.")

(defun ldb-scheme-guile-diff--normalize (s)
  "Canonicalise printed value S across the Scheme/Elisp surface gap."
  (let ((x (downcase (string-trim (or s "")))))
    (setq x (replace-regexp-in-string "#t" "t" x t t))
    (setq x (replace-regexp-in-string "#f" "nil" x t t))
    ;; empty list () -> nil (Emacs regexp: bare parens are literal)
    (setq x (replace-regexp-in-string "()" "nil" x t t))
    x))

(defun ldb-scheme-guile-diff--guile-eval (source probe)
  "Eval SOURCE in Guile and return PROBE's printed (write) value (string)."
  (let ((script (make-temp-file "ldb-guile-" nil ".scm")))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert source "\n"
                    "(display \"<<<\")(write " probe ")(display \">>>\")(newline)\n"))
          (with-temp-buffer
            (call-process ldb-scheme-guile-program nil '(t nil) nil "-s" script)
            (goto-char (point-min))
            (if (re-search-forward "<<<\\(\\(?:.\\|\n\\)*?\\)>>>" nil t)
                (match-string 1)
              (error "Guile produced no delimited value: %s" (buffer-string)))))
      (delete-file script))))

(defun ldb-scheme-guile-diff--bridge-eval (source probe)
  "Translate SOURCE+PROBE via the bridge, eval here, return printed value."
  (let* ((all (ldb-scheme-translate-string (concat source "\n" probe)))
         (defs (butlast all))
         (tail (car (last all))))
    (dolist (f defs) (eval f t))
    (prin1-to-string (eval tail t))))

(defmacro ldb-scheme-guile-diff-deftest (name source probe)
  "Define ERT differential test NAME comparing Guile vs bridge."
  (declare (indent 1))
  `(ert-deftest ,name ()
     (skip-unless ldb-scheme-guile-program)
     (should (equal (ldb-scheme-guile-diff--normalize
                     (ldb-scheme-guile-diff--guile-eval ,source ,probe))
                    (ldb-scheme-guile-diff--normalize
                     (ldb-scheme-guile-diff--bridge-eval ,source ,probe))))))

;;;; --- cases ----------------------------------------------------------------

(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/recursion
  "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
  "(fact 6)")

;; Lisp-1: a parameter called like a function -> funcall.
(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/hof-funcall
  "(define (apply-twice f x) (f (f x)))"
  "(apply-twice (lambda (y) (* y y)) 3)")

;; A procedure returned and immediately applied: ((adder 5) 10).
(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/closure-apply
  "(define (adder n) (lambda (x) (+ x n)))"
  "((adder 5) 10)")

(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/map-lambda
  ""
  "(map (lambda (x) (* x x)) (list 1 2 3 4))")

;; A named global function passed by value -> #'sq.
(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/map-named-fn
  "(define (sq x) (* x x))"
  "(map sq (list 1 2 3 4))")

(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/filter
  ""
  "(filter (lambda (x) (> x 2)) (list 1 2 3 4 5))")

(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/named-let
  ""
  "(let loop ((i 0) (acc 0)) (if (= i 5) acc (loop (+ i 1) (+ acc i))))")

;; letrec + mutual recursion + boolean result (tests #t/#f normalisation).
(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/letrec-mutual
  "(define (test n)
     (letrec ((ev? (lambda (m) (if (= m 0) #t (od? (- m 1)))))
              (od? (lambda (m) (if (= m 0) #f (ev? (- m 1))))))
       (ev? n)))"
  "(list (test 10) (test 7))")

(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/cond-else
  "(define (sign x) (cond ((< x 0) 'neg) ((> x 0) 'pos) (else 'zero)))"
  "(list (sign -3) (sign 4) (sign 0))")

(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/let-star
  ""
  "(let* ((a 2) (b (* a 3)) (c (+ a b))) (list a b c))")

(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/set-bang
  "(define counter 0)
   (define (bump!) (set! counter (+ counter 1)) counter)"
  "(list (bump!) (bump!) (bump!))")

(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/null-predicate
  ""
  "(list (null? '()) (null? (list 1)) (pair? (list 1)))")

(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/list-ops
  ""
  "(reverse (append (list 1 2) (list 3 4)))")

(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/string-append
  ""
  "(string-append \"ab\" \"cd\" \"ef\")")

;; Higher-order fold via named-let over a list passed in.
(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/fold-sum
  "(define (sum lst)
     (let loop ((xs lst) (acc 0))
       (if (null? xs) acc (loop (cdr xs) (+ acc (car xs))))))"
  "(sum (list 10 20 30 40))")

;; --- quasiquote ---
(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/quasiquote-unquote
  ""
  "(let ((b 5)) `(a ,b c))")

(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/quasiquote-splice
  ""
  "(let ((xs (list 1 2 3))) `(start ,@xs end))")

(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/quasiquote-computed
  ""
  "(let ((x 3)) `(square ,(* x x)))")

;; quasiquote in a real-code idiom: symbolic-sum constructor (deriv-style).
(ldb-scheme-guile-diff-deftest ldb-scheme-guile-diff/quasiquote-make-sum
  "(define (make-sum a b) `(+ ,a ,b))"
  "(make-sum 'x 3)")

(provide 'ldb-scheme-guile-diff)
;;; ldb-scheme-guile-diff.el ends here
