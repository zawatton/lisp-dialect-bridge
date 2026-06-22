;;; ldb-cl-alexandria-test.el --- Real alexandria via pre-expand+translate -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 4e validation on a REAL Common Lisp library: alexandria
;; (Debian cl-alexandria 20240125.git8514d8e).  Each case embeds an
;; *executable-verbatim* alexandria definition (docstrings trimmed, code
;; unchanged) and drives it through the production path:
;;
;;     real alexandria source  --SBCL pre-expand-->  core CL
;;                             --bridge translate-->  Elisp  --eval--> value
;;
;; and asserts the value agrees with SBCL running the same source (the
;; differential oracle from `ldb-cl-sbcl-diff').  This is the literal
;; "alexandria pre-expand -> translate" demonstration: three utility
;; functions (one self-contained, one labels+push+nreverse, one a &key
;; loop) and three real binding macros (if-let / when-let / when-let*,
;; which use destructuring &body, labels and backquote in their expanders).
;;
;; Gated on SBCL (`skip-unless'); folded into `make difftest'.
;;
;; Honest coverage boundary (probed against real alexandria, all REJECTED
;; *loudly* via `ldb-cl-unsupported-form-error' — never silently
;; mis-translated):
;;   * `proper-list-p' uses the `do' loop macro — not yet mapped.
;;   * `xor' expands (via alexandria's own `with-gensyms') to
;;     `block' / `return-from' / `values' — non-local exit + multiple
;;     values are unsupported.
;;   * `curry' / `compose' use `multiple-value-call' and a
;;     `define-compiler-macro'; `hash-table-keys' depends on another
;;     alexandria function (`maphash-keys').  All out for v1.
;; The seven covered definitions are the in-scope slice; the rejects above
;; are the documented frontier (do-loop, non-local exit, multiple values).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ldb-cl)
(require 'ldb-cl-macro)
(require 'ldb-cl-sbcl-diff)

(defun ldb-cl-alex-test--bridge (source probe)
  "Pre-expand SOURCE+PROBE via SBCL, translate, eval, return printed value."
  (let* ((expanded (ldb-cl-macro-expand-string (concat source "\n" probe)))
         (all (ldb-cl-translate-string expanded))
         (defs (butlast all))
         (tail (car (last all))))
    (dolist (f defs) (eval f t))
    (prin1-to-string (eval tail t))))

(defmacro ldb-cl-alex-difftest (name source probe)
  "Define ERT test NAME: SBCL on SOURCE vs bridge pre-expand+translate+eval."
  (declare (indent 1))
  `(ert-deftest ,name ()
     (skip-unless (ldb-cl-macro-preexpand-available-p))
     (should (equal (ldb-cl-sbcl-diff--normalize
                     (ldb-cl-sbcl-diff--sbcl-eval ,source ,probe))
                    (ldb-cl-sbcl-diff--normalize
                     (ldb-cl-alex-test--bridge ,source ,probe))))))

;;;; --- real alexandria sources (verbatim code) ------------------------------

(defconst ldb-cl-alex--ensure-list "\
(defun ensure-list (list)
  \"If LIST is a list, it is returned; else the list designated by LIST.\"
  (if (listp list)
      list
      (list list)))")

(defconst ldb-cl-alex--flatten "\
(defun flatten (tree)
  \"Traverses the tree in order, collecting non-null leaves into a list.\"
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))")

(defconst ldb-cl-alex--iota "\
(defun iota (n &key (start 0) (step 1))
  \"Return a list of N numbers from START stepping by STEP.\"
  (declare (type (integer 0) n) (number start step))
  (loop for i = (+ (- (+ start step) step)) then (+ i step)
        repeat n
        collect i))")

(defconst ldb-cl-alex--clamp "\
(defun clamp (number min max)
  \"Clamps NUMBER into the [MIN, MAX] range.\"
  (if (< number min)
      min
      (if (> number max)
          max
          number)))")

(defconst ldb-cl-alex--if-let "\
(defmacro if-let (bindings &body (then-form &optional else-form))
  \"Bind BINDINGS, run THEN-FORM if all are true else ELSE-FORM.\"
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           ,then-form
           ,else-form))))")

(defconst ldb-cl-alex--when-let "\
(defmacro when-let (bindings &body forms)
  \"Bind BINDINGS, run FORMS as progn when all bindings are true.\"
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))")

(defconst ldb-cl-alex--when-let* "\
(defmacro when-let* (bindings &body body)
  \"Bind BINDINGS sequentially, short-circuiting on nil; run BODY as progn.\"
  (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                          (list bindings)
                          bindings)))
    (labels ((bind (bindings body)
               (if bindings
                   `(let (,(car bindings))
                      (when ,(caar bindings)
                        ,(bind (cdr bindings) body)))
                   `(progn ,@body))))
      (bind binding-list body))))")

;;;; --- functions ------------------------------------------------------------

(ldb-cl-alex-difftest ldb-cl-alex/ensure-list-atom
  ldb-cl-alex--ensure-list "(ensure-list 5)")

(ldb-cl-alex-difftest ldb-cl-alex/ensure-list-list
  ldb-cl-alex--ensure-list "(ensure-list (list 1 2 3))")

(ldb-cl-alex-difftest ldb-cl-alex/flatten
  ldb-cl-alex--flatten "(flatten (quote (1 (2 (3 4) 5) (6))))")

(ldb-cl-alex-difftest ldb-cl-alex/iota-default
  ldb-cl-alex--iota "(iota 4)")

(ldb-cl-alex-difftest ldb-cl-alex/iota-keys
  ldb-cl-alex--iota "(iota 3 :start 1 :step 2)")

(ldb-cl-alex-difftest ldb-cl-alex/clamp-low
  ldb-cl-alex--clamp "(clamp -3 0 10)")

(ldb-cl-alex-difftest ldb-cl-alex/clamp-mid
  ldb-cl-alex--clamp "(clamp 12 0 10)")

;;;; --- macros (pre-expanded) ------------------------------------------------

(ldb-cl-alex-difftest ldb-cl-alex/if-let-then
  ldb-cl-alex--if-let "(if-let ((a 5)) (* a 2) :none)")

(ldb-cl-alex-difftest ldb-cl-alex/if-let-else
  ldb-cl-alex--if-let "(if-let ((a nil)) (* a 2) :none)")

(ldb-cl-alex-difftest ldb-cl-alex/when-let-multi
  ldb-cl-alex--when-let "(when-let ((a 3) (b 4)) (+ a b))")

(ldb-cl-alex-difftest ldb-cl-alex/when-let-shortcircuit
  ldb-cl-alex--when-let "(when-let ((a 3) (b nil)) (+ a b))")

(ldb-cl-alex-difftest ldb-cl-alex/when-let*-sequential
  ldb-cl-alex--when-let* "(when-let* ((a 2) (b (* a 3))) (+ a b))")

(provide 'ldb-cl-alexandria-test)
;;; ldb-cl-alexandria-test.el ends here
