(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :array-operations)
  (ql:quickload :rove))

(defpackage :dezero-naive.steps.step10
  (:use :common-lisp)
  (:shadow :exp)
  (:export
   :backward
   :<variable>
   :@data
   :@gradient
   :square
   :<square>
   :numerica-diff))
(in-package :dezero-naive.steps.step10)

(defun full-like (array fill-value)
  (let ((dims (array-dimensions array)))
    (make-array dims :initial-element fill-value)))

(defun zeros-like (array)
  (full-like array 0))

(defun ones-like (array)
  (full-like array 1))

(defun ensure-array (x)
  (if (numberp x)
      (vector x)
      x))


(defgeneric call (function input))

(defgeneric forward (function x))

(defgeneric backward (function-or-variable &optional gy))


(defclass <variable> ()
  ((data :initarg :data :accessor @data)
   (gradient :initform nil :accessor @gradient)
   (creator :initform nil :accessor @creator)))

(defmethod initialize-instance :after ((var <variable>) &key)
  (check-type (@data var) array))

(defun <variable> (data)
  (make-instance '<variable> :data data))

(defclass <function> ()
  ((input :initform nil :accessor @input)
   (output :initform nil :accessor @output)))

(defun <function> ())


(defmethod set-creator ((var <variable>) func)
  (setf (@creator var) func))

(defmethod backward ((var <variable>) &optional gy)
  (declare (ignore gy))

  (unless (@gradient var)
    (setf (@gradient var) (ones-like (@data var))))

  (let ((funcs (list (@creator var))))
    (loop :while funcs
          :do (let* ((func (pop funcs))
                     (x (@input func))
                     (y (@output func)))
                (setf (@gradient x) (backward func (@gradient y)))
                (when (@creator x)
                  (push (@creator x) funcs))))))


(defmethod call ((func <function>) input)
  (let* ((x (@data input))
         (y (forward func x))
         (output (<variable> (ensure-array y))))
    (set-creator output func)
    (setf (@input func) input)
    (setf (@output func) output)
    output))

(defmethod forward ((function <function>) x)
  (error "not implemented"))

(defmethod backward ((function <function>) &optional x)
  (declare (ignore x))
  (error "not implemented"))


(defclass <square> (<function>) ())

(defun <square> () (make-instance '<square>))

(defun square (x)
  (call (<square>) x))

(defmethod forward ((func <square>) x)
  (aops:vectorize (x)
    (* x x)))

(defmethod backward ((func <square>) &optional gy)
  (let* ((x (@data (@input func)))
         (gy (aops:vectorize (x gy) (* x gy 2.0d0))))
    gy))



(defclass <exp> (<function>) ())

(defun <exp> () (make-instance '<exp>))

(defun exp (x)
  (call (<exp>) x))

(defmethod forward ((func <exp>) x)
  (aops:vectorize (x)
    (cl:exp x)))

(defmethod backward ((func <exp>) &optional gy)
  (let* ((x (@data (@input func)))
         (gx (aops:vectorize (x gy) (* (cl:exp x) gy))))
    gx))


(defun numerical-diff (f x &optional (eps 1.0d-4))
  (let* ((x (@data x))
         (x0 (<variable> (aops:vectorize (x) (- x eps))))
         (x1 (<variable> (aops:vectorize (x) (+ x eps))))
         (y0 (@data (call f x0)))
         (y1 (@data (call f x1))))
    (aops:vectorize (y1 y0)
      (/ (- y1 y0)
         (* 2 eps)))))

(defpackage :dezero-naive.steps.step10.test
  (:use :common-lisp :rove)
  (:import-from :dezero-naive.steps.step10
   :backward
   :<variable>
   :@data
   :@gradient
   :square
   :<square>
   :numerical-diff))
(in-package :dezero-naive.steps.step10.test)

(defun all-close (x y)
  (every (lambda (x) (<= x 1d-08))
         (aops:flatten
          (aops:vectorize (x y)
            (/ (abs (- x y)) (abs x))))))


(deftest square-test
  (testing "test forward"
    (let* ((x (<variable> #(2.0)))
           (y (square x))
           (expected #(4.0)))
      (ok (equalp (@data y) expected))))
  (testing "test backward"
    (let* ((x (<variable> #(3.0)))
           (y (square x)))
      (backward y)
      (let ((expected #(6.0)))
        (ok (equalp (@gradient x) expected)))))
  (testing "test gradient check"
    (let* ((x (<variable> (aops:generate (lambda () (random 1.0d0)) 1)))
           (y (square x)))
      (backward y)
      (let ((num-grad (numerical-diff (<square>) x)))
        (ok (all-close (@gradient x) num-grad))))))

(run-suite :dezero-naive.steps.step10.test)
