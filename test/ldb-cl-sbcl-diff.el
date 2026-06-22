;;; ldb-cl-sbcl-diff.el --- Differential tests: SBCL oracle vs bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Treats SBCL as the ground-truth oracle for Common Lisp semantics.  Each
;; case runs one real CL program two ways:
;;
;;   1. SBCL `--script' on the original source, printing the probe value
;;      between `<<<' / `>>>' sentinels.
;;   2. lisp-dialect-bridge translates the same source to Elisp, this Emacs
;;      evals it, and the probe value is `prin1'-ed.
;;
;; Both printed values are normalised (trim + downcase, which folds away the
;; CL upcase / `T'/`NIL' vs `t'/`nil' differences) and asserted equal.  A
;; mismatch is a concrete translation bug, not a subjective judgement — this
;; upgrades the earlier eyeball/PAIP-style validation to a mechanical
;; differential test.  All cases skip when sbcl is absent from PATH.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'eieio)
(require 'ldb-cl)
(require 'ldb-emit-elisp)

(defvar ldb-cl-sbcl-diff--sbcl
  (or (executable-find "sbcl")
      (let ((p "/usr/bin/sbcl")) (and (file-executable-p p) p)))
  "Absolute path to SBCL, or nil when unavailable.")

(defun ldb-cl-sbcl-diff--normalize (s)
  "Canonicalise printed value S for cross-dialect comparison."
  (downcase (string-trim (or s ""))))

(defun ldb-cl-sbcl-diff--sbcl-eval (cl-source probe)
  "Eval CL-SOURCE in SBCL and return PROBE's printed value (string)."
  (let ((script (make-temp-file "ldb-sbcl-" nil ".lisp")))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert cl-source "\n"
                    (format "(format t \"<<<~S>>>~%%\" %s)\n" probe)))
          (with-temp-buffer
            ;; stderr discarded so compiler notes never pollute capture.
            (call-process ldb-cl-sbcl-diff--sbcl nil '(t nil) nil
                          "--script" script)
            (goto-char (point-min))
            (if (re-search-forward "<<<\\(\\(?:.\\|\n\\)*?\\)>>>" nil t)
                (match-string 1)
              (error "SBCL produced no delimited value: %s"
                     (buffer-string)))))
      (delete-file script))))

(defun ldb-cl-sbcl-diff--bridge-eval (cl-source probe)
  "Translate CL-SOURCE+PROBE via the bridge, eval here, return printed value.
All but the final translated form are evaluated for effect (the defs); the
final form is the probe whose value is `prin1'-ed."
  (let* ((all (ldb-cl-translate-string (concat cl-source "\n" probe)))
         (defs (butlast all))
         (tail (car (last all))))
    (dolist (f defs) (eval f t))
    (prin1-to-string (eval tail t))))

(defun ldb-cl-sbcl-diff--pair (cl-source probe)
  "Return (CL-OUT . EL-OUT) normalised pair for CL-SOURCE / PROBE."
  (cons (ldb-cl-sbcl-diff--normalize
         (ldb-cl-sbcl-diff--sbcl-eval cl-source probe))
        (ldb-cl-sbcl-diff--normalize
         (ldb-cl-sbcl-diff--bridge-eval cl-source probe))))

(defmacro ldb-cl-sbcl-diff-deftest (name cl-source probe)
  "Define ERT differential test NAME comparing SBCL vs bridge.
CL-SOURCE is the program (defuns etc.); PROBE is the CL expression whose
value is compared.  `skip-unless' is expanded inline per ERT rules."
  (declare (indent 1))
  `(ert-deftest ,name ()
     (skip-unless ldb-cl-sbcl-diff--sbcl)
     (let ((r (ldb-cl-sbcl-diff--pair ,cl-source ,probe)))
       (should (equal (car r) (cdr r))))))

;;;; --- cases ----------------------------------------------------------------

(ldb-cl-sbcl-diff-deftest ldb-cl-sbcl-diff/arith-let
  "(defun sq (x) (let ((y (* x x))) y))"
  "(sq 7)")

(ldb-cl-sbcl-diff-deftest ldb-cl-sbcl-diff/recursion-cond
  "(defun fact (n) (cond ((<= n 1) 1) (t (* n (fact (- n 1))))))"
  "(fact 6)")

(ldb-cl-sbcl-diff-deftest ldb-cl-sbcl-diff/loop-sum
  "(defun s (n) (loop for i from 1 to n sum i))"
  "(s 100)")

(ldb-cl-sbcl-diff-deftest ldb-cl-sbcl-diff/loop-collect
  ""
  "(loop for i from 1 to 4 collect (* i i))")

(ldb-cl-sbcl-diff-deftest ldb-cl-sbcl-diff/reduce-initial
  ""
  "(reduce (function +) (list 1 2 3) :initial-value 100)")

(ldb-cl-sbcl-diff-deftest ldb-cl-sbcl-diff/case
  "(defun classify (x) (case x (1 :one) (2 :two) (t :other)))"
  "(classify 2)")

(ldb-cl-sbcl-diff-deftest ldb-cl-sbcl-diff/defstruct
  "(defstruct pt a b)"
  "(let ((p (make-pt :a 3 :b 4))) (+ (pt-a p) (pt-b p)))")

;; NB: class name avoids shadowing an Emacs core function.  A class literally
;; named `point' makes eieio (backward-compat constructor) clobber the
;; built-in `(point)', which then crashes unrelated code — a real translation
;; hazard recorded in docs/design/02-cl-core.org.
(ldb-cl-sbcl-diff-deftest ldb-cl-sbcl-diff/clos
  "(defclass vec2 () ((x :initarg :x :accessor vx) (y :initarg :y :accessor vy)))
(defmethod dist-sq ((p vec2)) (+ (* (vx p) (vx p)) (* (vy p) (vy p))))"
  "(dist-sq (make-instance 'vec2 :x 3 :y 4))")

(ldb-cl-sbcl-diff-deftest ldb-cl-sbcl-diff/handler-case-caught
  "(defun safe (x y) (handler-case (/ x y) (division-by-zero () -1)))"
  "(safe 10 0)")

(ldb-cl-sbcl-diff-deftest ldb-cl-sbcl-diff/handler-case-normal
  "(defun safe (x y) (handler-case (/ x y) (division-by-zero () -1)))"
  "(safe 10 2)")

(provide 'ldb-cl-sbcl-diff)
;;; ldb-cl-sbcl-diff.el ends here
