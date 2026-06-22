;;; ldb-cl-test.el --- ERT for the CL core-syntax translator -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; This file is part of lisp-dialect-bridge.  GPL-3.0-or-later.

;;; Commentary:

;; Phase 4a coverage.  Golden tests assert the emitted Elisp form; the
;; behavioural tests `eval' the emitted Elisp (cl-lib loaded) and check
;; runtime results — proving the translation actually *runs*, not just
;; pretty-prints.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lisp-dialect-bridge)

(defun ldb-cl-test--eval (cl-src)
  "Translate CL-SRC, `eval' every emitted form, return the last value."
  (let ((val nil))
    (dolist (f (ldb-cl-translate-string cl-src) val)
      (setq val (eval f t)))))

;;;; --- reader ---------------------------------------------------------------

(ert-deftest ldb-cl-test-read-char-literal ()
  "#\\X / #\\Newline read to the right Elisp character codes."
  (should (equal ?A (ldb-cl-read-from-string "#\\A")))
  (should (equal 10 (ldb-cl-read-from-string "#\\Newline")))
  (should (equal ?\( (ldb-cl-read-from-string "#\\("))))

;;;; --- golden translations ---------------------------------------------------

(ert-deftest ldb-cl-test-defun-factorial-golden ()
  "defun + if + recursion translate near-identically."
  (should (equal
           '(defun fact (n) (if (= n 0) 1 (* n (fact (1- n)))))
           (ldb-cl-translate-form
            (ldb-cl-read-from-string
             "(defun fact (n) (if (= n 0) 1 (* n (fact (1- n)))))")))))

(ert-deftest ldb-cl-test-let*-cond-golden ()
  "let* + cond translate verbatim (Lisp-2, shared forms)."
  (should (equal
           '(let* ((a 1) (b (+ a 1))) (cond ((> b 1) b) (t 0)))
           (ldb-cl-translate-form
            (ldb-cl-read-from-string
             "(let* ((a 1) (b (+ a 1))) (cond ((> b 1) b) (t 0)))")))))

(ert-deftest ldb-cl-test-seq-fn-remap-golden ()
  "reduce/remove-if remap to cl-reduce/cl-remove-if; #'+ stays."
  (should (equal
           '(cl-reduce #'+ (list 1 2 3))
           (ldb-cl-translate-form
            (ldb-cl-read-from-string "(reduce #'+ (list 1 2 3))")))))

(ert-deftest ldb-cl-test-strip-declare ()
  "(declare ...) is dropped; the rest of the body is preserved."
  (should (equal
           '(defun k (n) (* n 2))
           (ldb-cl-translate-form
            (ldb-cl-read-from-string
             "(defun k (n) (declare (integer n)) (* n 2))")))))

(ert-deftest ldb-cl-test-extended-lambda-list-uses-cl-defun ()
  "&optional-with-default / &key force cl-defun."
  (should (eq 'cl-defun
             (car (ldb-cl-translate-form
                   (ldb-cl-read-from-string
                    "(defun g (a &optional (b 10) &key c) (+ a b (if c c 0)))")))))
  (should (eq 'defun
             (car (ldb-cl-translate-form
                   (ldb-cl-read-from-string "(defun s (a b) (+ a b)))"))))))

;;;; --- behavioural (eval the emitted Elisp) ---------------------------------

(ert-deftest ldb-cl-test-eval-factorial ()
  "Translated recursive defun runs and returns the right value."
  (ldb-cl-test--eval
   "(defun ldb-cl-test-fact (n) (if (= n 0) 1 (* n (ldb-cl-test-fact (1- n)))))")
  (should (= 120 (funcall 'ldb-cl-test-fact 5)))
  (fmakunbound 'ldb-cl-test-fact))

(ert-deftest ldb-cl-test-eval-labels-recursion ()
  "cl-labels local recursion runs."
  (should (= 120
             (ldb-cl-test--eval
              "(labels ((f (n) (if (= n 0) 1 (* n (f (1- n)))))) (f 5))"))))

(ert-deftest ldb-cl-test-eval-extended-lambda-list ()
  "cl-defun with &optional default + &key behaves correctly."
  (ldb-cl-test--eval
   "(defun ldb-cl-test-g (a &optional (b 10) &key c) (+ a b (if c c 0)))")
  (should (= 11 (funcall 'ldb-cl-test-g 1)))
  (should (= 6 (funcall 'ldb-cl-test-g 1 2 :c 3)))
  (fmakunbound 'ldb-cl-test-g))

(ert-deftest ldb-cl-test-eval-setf-incf ()
  "setf place mutation and incf->cl-incf run."
  (should (equal '(10 7 3)
                 (ldb-cl-test--eval
                  "(let ((x (list 1 2 3))) (setf (car x) 10) (incf (cadr x) 5) x)"))))

(ert-deftest ldb-cl-test-eval-seq-fns ()
  "reduce/remove-if remaps run with cl-lib semantics."
  (should (= 6 (ldb-cl-test--eval "(reduce #'+ (list 1 2 3))")))
  (should (equal '(2 4)
                 (ldb-cl-test--eval
                  "(remove-if (lambda (n) (oddp n)) (list 1 2 3 4))"))))

(ert-deftest ldb-cl-test-eval-dolist ()
  "dolist accumulation runs (binding spec handled)."
  (should (= 6
             (ldb-cl-test--eval
              "(let ((s 0)) (dolist (x (list 1 2 3) s) (setq s (+ s x))))"))))

;;;; --- reject list ----------------------------------------------------------

(ert-deftest ldb-cl-test-defclass-golden ()
  "defclass -> eieio defclass; :initform translated, other options kept."
  (should (equal '(defclass point () ((x :initarg :x :initform 0 :accessor point-x)))
                 (ldb-cl-translate-form
                  (ldb-cl-read-from-string
                   "(defclass point () ((x :initarg :x :initform 0 :accessor point-x)))")))))

(ert-deftest ldb-cl-test-defmethod-golden ()
  "defmethod -> cl-defmethod; specialized arglist kept, body translated."
  (should (equal '(cl-defmethod area ((c circle)) (* 3 (radius c) (radius c)))
                 (ldb-cl-translate-form
                  (ldb-cl-read-from-string
                   "(defmethod area ((c circle)) (* 3 (radius c) (radius c)))"))))
  (should (equal '(cl-defgeneric area (shape))
                 (ldb-cl-translate-form
                  (ldb-cl-read-from-string "(defgeneric area (shape))")))))

(ert-deftest ldb-cl-test-eval-clos ()
  "A CLOS class + method translates and runs via eieio / cl-defmethod."
  (require 'eieio)
  (ldb-cl-test--eval
   "(defclass ldb-cl-test-pt () ((x :initarg :x :initform 0 :accessor ldb-cl-test-pt-x) (y :initarg :y :initform 0 :accessor ldb-cl-test-pt-y)))")
  (ldb-cl-test--eval
   "(defmethod ldb-cl-test-norm ((p ldb-cl-test-pt)) (sqrt (+ (* (ldb-cl-test-pt-x p) (ldb-cl-test-pt-x p)) (* (ldb-cl-test-pt-y p) (ldb-cl-test-pt-y p)))))")
  (should (= 5.0 (funcall 'ldb-cl-test-norm
                          (make-instance 'ldb-cl-test-pt :x 3 :y 4)))))

(ert-deftest ldb-cl-test-handler-case-golden ()
  "handler-case -> condition-case (single var, body translated)."
  (should (equal '(condition-case e (signal-it) (error (list 'caught e)))
                 (ldb-cl-translate-form
                  (ldb-cl-read-from-string
                   "(handler-case (signal-it) (error (e) (list 'caught e)))")))))

(ert-deftest ldb-cl-test-eval-handler-case ()
  "Translated handler-case catches an error / passes the value through,
and per-clause vars collapse onto one condition-case var (let-rebind)."
  (should (eq 'caught (ldb-cl-test--eval
                       "(handler-case (error \"boom\") (error (e) 'caught))")))
  (should (= 7 (ldb-cl-test--eval "(handler-case (+ 3 4) (error (e) 0))")))
  (should (eq 'second
             (ldb-cl-test--eval
              "(handler-case (error \"e\") (arithmetic-error (a) 'first) (error (b) 'second))"))))

(ert-deftest ldb-cl-test-reject-defmacro ()
  "defmacro signals (macros gated on hygiene decision)."
  (should-error (ldb-cl-translate-string "(defmacro m (x) `(+ ,x 1))")
                :type 'ldb-cl-unsupported-form-error))

(ert-deftest ldb-cl-test-loop ()
  "loop -> cl-loop; embedded expressions translated/remapped, keywords kept."
  (should (equal '(cl-loop for i from 1 to 3 collect (* i i))
                 (ldb-cl-translate-form
                  (ldb-cl-read-from-string "(loop for i from 1 to 3 collect (* i i))"))))
  (should (equal '(1 9 25)
                 (ldb-cl-test--eval "(loop for i in (list 1 3 5) collect (* i i))")))
  (should (equal '(2 4)
                 (ldb-cl-test--eval
                  "(loop for i in (list 1 2 3 4) when (evenp i) collect i)"))))

(ert-deftest ldb-cl-test-multiple-values ()
  "multiple-value-bind + a multiple-valued function (floor -> cl-floor)."
  (should (= 5 (ldb-cl-test--eval
                "(multiple-value-bind (q r) (floor 17 4) (+ q r))"))))

(ert-deftest ldb-cl-test-eval-case ()
  "CL case maps to cl-case with clause keys preserved."
  (ldb-cl-test--eval
   "(defun ldb-cl-test-kd (e) (case (first e) (+ 'sum) (* 'product) (otherwise 'other)))")
  (should (eq 'sum (funcall 'ldb-cl-test-kd '(+ a b))))
  (should (eq 'other (funcall 'ldb-cl-test-kd '(/ a b))))
  (fmakunbound 'ldb-cl-test-kd))

(ert-deftest ldb-cl-test-eval-char-aref ()
  "CL char (string indexing) maps to aref."
  (ldb-cl-test--eval
   "(defun ldb-cl-test-vp (x) (and (symbolp x) (eql (char (symbol-name x) 0) #\\?)))")
  (should (funcall 'ldb-cl-test-vp (intern "?foo")))
  (should-not (funcall 'ldb-cl-test-vp 'foo))
  (fmakunbound 'ldb-cl-test-vp))

(ert-deftest ldb-cl-test-function-designator ()
  "#'cl-builtin is remapped inside (function ...); user fns pass through."
  (should (equal '(cl-remove-if #'cl-oddp (list 1 2 3 4))
                 (ldb-cl-translate-form
                  (ldb-cl-read-from-string "(remove-if #'oddp (list 1 2 3 4))"))))
  (should (equal '(2 4)
                 (ldb-cl-test--eval "(remove-if #'oddp (list 1 2 3 4))"))))

(ert-deftest ldb-cl-test-eval-defstruct ()
  "defstruct -> cl-defstruct; constructor/accessors work."
  (should (equal '(cl-defstruct ldb-cl-test-pt (x 0) (y 0))
                 (ldb-cl-translate-form
                  (ldb-cl-read-from-string "(defstruct ldb-cl-test-pt (x 0) (y 0))"))))
  (ldb-cl-test--eval "(defstruct ldb-cl-test-pt (x 0) (y 0))")
  (should (= 3 (ldb-cl-test--eval
                "(ldb-cl-test-pt-x (make-ldb-cl-test-pt :x 3 :y 4))"))))

(ert-deftest ldb-cl-test-format-nil-golden ()
  "format nil + ~a/~s/~% directives -> Elisp format control string."
  (should (equal '(format "x=%s, y=%S\n" a b)
                 (ldb-cl-translate-form
                  (ldb-cl-read-from-string "(format nil \"x=~a, y=~s~%\" a b)")))))

(ert-deftest ldb-cl-test-format-t-golden ()
  "format t -> (princ (format ...))."
  (should (equal '(princ (format "%s\n" x))
                 (ldb-cl-translate-form
                  (ldb-cl-read-from-string "(format t \"~a~%\" x)")))))

(ert-deftest ldb-cl-test-eval-format ()
  "Translated format nil runs and returns the right string."
  (should (equal "1+2=3" (ldb-cl-test--eval "(format nil \"~a+~a=~a\" 1 2 3)")))
  (should (equal "100%" (ldb-cl-test--eval "(format nil \"~a%\" 100)"))))

(ert-deftest ldb-cl-test-reject-format-param ()
  "Parameterized directive (~5d) is rejected loudly."
  (should-error (ldb-cl-translate-string "(format nil \"~5d\" 3)")
                :type 'ldb-cl-unsupported-form-error))

(ert-deftest ldb-cl-test-reject-format-stream ()
  "A stream destination (non nil/t) is rejected loudly."
  (should-error (ldb-cl-translate-string "(format s \"~a\" x)")
                :type 'ldb-cl-unsupported-form-error))

(ert-deftest ldb-cl-test-backquote-golden ()
  "Backquote keeps literal template; unquoted exprs are translated/remapped."
  (should (equal '`(+ ,x ,(cl-incf y) 1)
                 (ldb-cl-translate-form
                  (ldb-cl-read-from-string "`(+ ,x ,(incf y) 1)")))))

(ert-deftest ldb-cl-test-eval-backquote ()
  "Translated backquote builds the right structure at runtime."
  (should (equal '(+ 5 1) (ldb-cl-test--eval "(let ((x 5)) `(+ ,x 1))")))
  (should (equal '(a b c d)
                 (ldb-cl-test--eval "(let ((xs '(b c))) `(a ,@xs d))"))))

(ert-deftest ldb-cl-test-reject-nested-backquote ()
  "Nested backquote is rejected loudly (core v1)."
  (should-error (ldb-cl-translate-string "`(a `(b ,c))")
                :type 'ldb-cl-unsupported-form-error))

(ert-deftest ldb-cl-test-reject-destructuring-bind ()
  "destructuring-bind (a binding form, not core) signals."
  (should-error (ldb-cl-translate-string "(destructuring-bind (a b) lst (+ a b))")
                :type 'ldb-cl-unsupported-form-error))

(ert-deftest ldb-cl-test-do-golden ()
  "do -> cl-do with translated init/step/end/result; vars verbatim."
  (should (equal '(cl-do ((i 0 (1+ i)) (acc 0 (+ acc i))) ((= i 5) acc))
                 (ldb-cl-translate-form
                  (ldb-cl-read-from-string
                   "(do ((i 0 (1+ i)) (acc 0 (+ acc i))) ((= i 5) acc))")))))

(ert-deftest ldb-cl-test-do-eval ()
  "Translated do accumulates correctly at runtime."
  (should (= 10 (ldb-cl-test--eval
                 "(do ((i 0 (1+ i)) (acc 0 (+ acc i))) ((= i 5) acc))"))))

(ert-deftest ldb-cl-test-do*-sequential-eval ()
  "do* -> cl-do* with sequential binding (b sees a's stepped value)."
  (should (= 40 (ldb-cl-test--eval
                 "(do* ((a 1 (1+ a)) (b (* a 10) (* a 10))) ((> a 3) b))"))))

(ert-deftest ldb-cl-test-do-return-eval ()
  "return inside do exits the implicit nil block (via cl-return)."
  (should (= 300 (ldb-cl-test--eval
                  "(do ((i 0 (1+ i))) (nil) (when (= i 3) (return (* i 100))))"))))

(ert-deftest ldb-cl-test-block-return-from-golden ()
  "Named block/return-from -> cl-block/cl-return-from; names round-trip."
  (should (equal '(cl-block found
                    (dolist (x lst) (when (cl-evenp x) (cl-return-from found x)))
                    nil)
                 (ldb-cl-translate-form
                  (ldb-cl-read-from-string
                   "(block found (dolist (x lst) (when (evenp x) (return-from found x))) nil)")))))

(ert-deftest ldb-cl-test-block-return-from-eval ()
  "Named block early exit runs; normal fall-through and nested exit too."
  (should (= 4 (ldb-cl-test--eval
                "(block found (dolist (x (list 1 3 4 5)) (when (evenp x) (return-from found x))) nil)")))
  (should (= 3 (ldb-cl-test--eval "(block b (+ 1 2))")))
  (should (= 42 (ldb-cl-test--eval
                 "(block outer (block inner (return-from outer 42)) 99)"))))

(ert-deftest ldb-cl-test-reject-multiple-value-call ()
  "multiple-value-call is rejected loudly (Emacs cl-lib cannot splice values)."
  (should-error (ldb-cl-translate-string "(multiple-value-call #'+ (values 1 2))")
                :type 'ldb-cl-unsupported-form-error))

(provide 'ldb-cl-test)
;;; ldb-cl-test.el ends here
