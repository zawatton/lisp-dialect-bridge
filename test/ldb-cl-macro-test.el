;;; ldb-cl-macro-test.el --- Tests for CL macro pre-expansion -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 4e: exercises `ldb-cl-macro' (SBCL selective macro pre-expansion).
;; Two kinds of checks, all gated on SBCL being on PATH (`skip-unless'):
;;
;;   * shape / end-to-end: the expander drops `defmacro' and inlines the
;;     expansion; the full translate-and-run path yields the right value.
;;   * differential: run the original macro program in SBCL and the
;;     (pre-expand -> translate -> eval) path in Emacs, assert agreement.
;;     Reuses the SBCL-oracle helpers from `ldb-cl-sbcl-diff'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ldb-cl)
(require 'ldb-cl-macro)
(require 'ldb-cl-sbcl-diff)

(defun ldb-cl-macro-test--bridge-run (cl-source probe)
  "Pre-expand CL-SOURCE+PROBE via SBCL, translate, eval, return printed value."
  (let* ((expanded (ldb-cl-macro-expand-string (concat cl-source "\n" probe)))
         (all (ldb-cl-translate-string expanded))
         (defs (butlast all))
         (tail (car (last all))))
    (dolist (f defs) (eval f t))
    (prin1-to-string (eval tail t))))

(defmacro ldb-cl-macro-test-difftest (name cl-source probe)
  "Define ERT test NAME: SBCL on original vs bridge pre-expand+translate+eval."
  (declare (indent 1))
  `(ert-deftest ,name ()
     (skip-unless (ldb-cl-macro-preexpand-available-p))
     (should (equal (ldb-cl-sbcl-diff--normalize
                     (ldb-cl-sbcl-diff--sbcl-eval ,cl-source ,probe))
                    (ldb-cl-sbcl-diff--normalize
                     (ldb-cl-macro-test--bridge-run ,cl-source ,probe))))))

;;;; --- unit: availability / shape -------------------------------------------

(ert-deftest ldb-cl-macro/availability ()
  (skip-unless (ldb-cl-macro-preexpand-available-p))
  (should (file-executable-p (ldb-cl-macro-preexpand-available-p))))

(ert-deftest ldb-cl-macro/expand-drops-defmacro-and-inlines ()
  (skip-unless (ldb-cl-macro-preexpand-available-p))
  (let ((out (ldb-cl-macro-expand-string
              "(defmacro my-square (x) `(* ,x ,x))\n(defun f (a) (my-square a))")))
    ;; defmacro definition is dropped; standard `defun' survives; the macro
    ;; use is expanded to (* a a) with downcased symbols.
    (should-not (string-match-p "defmacro" out))
    (should (string-match-p "defun f" out))
    (should (string-match-p "(\\* a a)" out))))

(ert-deftest ldb-cl-macro/expand-keeps-standard-macros ()
  (skip-unless (ldb-cl-macro-preexpand-available-p))
  ;; `loop' and `when' are standard CL macros: they must NOT be expanded
  ;; (the existing 4a-4d' handlers translate them).
  (let ((out (ldb-cl-macro-expand-string
              "(defun s (n) (loop for i from 1 to n when (oddp i) sum i))")))
    (should (string-match-p "loop" out))
    (should (string-match-p "when" out))))

(ert-deftest ldb-cl-macro/expand-no-sbcl-signals ()
  ;; Force the unavailable branch regardless of host: a nil program signals.
  (let ((ldb-cl-sbcl-program nil))
    (should-error (ldb-cl-macro-expand-string "(defun f () 1)")
                  :type 'ldb-cl-macro-error)))

(ert-deftest ldb-cl-macro/end-to-end-eval ()
  (skip-unless (ldb-cl-macro-preexpand-available-p))
  (let ((forms (ldb-cl-translate-string-expanding
                "(defmacro my-square (x) `(* ,x ,x))\n(defun sq (a) (my-square a))")))
    (dolist (f forms) (eval f t))
    (should (= (funcall 'sq 9) 81))))

;;;; --- differential: SBCL oracle vs pre-expanded bridge ---------------------

(ldb-cl-macro-test-difftest ldb-cl-macro/diff-simple
  "(defmacro my-square (x) `(* ,x ,x))"
  "(my-square 6)")

(ldb-cl-macro-test-difftest ldb-cl-macro/diff-body-macro
  "(defmacro unless-zero (n &body body) `(if (/= ,n 0) (progn ,@body) 0))
(defun g (n) (unless-zero n (* n n)))"
  "(g 7)")

(ldb-cl-macro-test-difftest ldb-cl-macro/diff-macro-generates-defun
  "(defmacro def-adder (name k) `(defun ,name (x) (+ x ,k)))
(def-adder add5 5)"
  "(add5 10)")

(ldb-cl-macro-test-difftest ldb-cl-macro/diff-nested-user-macros
  "(defmacro my-square (x) `(* ,x ,x))
(defmacro sum-squares (a b) `(+ (my-square ,a) (my-square ,b)))"
  "(sum-squares 3 4)")

(ldb-cl-macro-test-difftest ldb-cl-macro/diff-macro-over-loop
  "(defmacro my-square (x) `(* ,x ,x))
(defun h (n) (loop for i from 1 to n sum (my-square i)))"
  "(h 4)")

(provide 'ldb-cl-macro-test)
;;; ldb-cl-macro-test.el ends here
