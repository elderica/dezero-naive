(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :array-operations))

(defpackage :dezero-naive.steps.step06
  (:use :common-lisp))
(in-package :dezero-naive.steps.step06)

(defgeneric call (function input))

(defgeneric forward (function x))

(defgeneric backward (func-or-var gy))

(defclass <variable> ()
  ((data :initarg :data :accessor @data)
   (gradient :initform nil :accessor @gradient)))

(defun <variable> (data)
  (make-instance '<variable> :data data))

(defclass <function> ()
  ((input :accessor @input)))

(defun <function> ())

(defmethod call ((func <function>) input)
  (let* ((x (@data input))
         (y (forward func x))
         (output (<variable> y)))
    (setf (@input func) input)
    output))

(defmethod forward ((function <function>) x)
  (error "not implemented"))

(defmethod backward ((function <function>) x)
  (error "not implemented"))

(defclass <square> (<function>)
  ())

(defun <square> ()
  (make-instance '<square>))

(defmethod forward ((func <square>) x)
  (aops:vectorize (x)
    (* x x)))

(defmethod backward ((func <square>) gy)
  (let* ((x (@data (@input func)))
         (gy (aops:vectorize (x gy) (* x gy 2.0d0))))
    gy))

(defclass <exp> (<function>)
  ())

(defun <exp> ()
  (make-instance '<exp>))

(defmethod forward ((func <exp>) x)
  (aops:vectorize (x)
    (exp x)))

(defmethod backward ((func <exp>) gy)
  (let* ((x (@data (@input func)))
         (gx (aops:vectorize (x gy) (* (exp x) gy))))
    gx))

(let ((af (<square>))
      (bf (<exp>))
      (cf (<square>)))
  (let* ((x (<variable> #(0.5d0)))
         (a (call af x))
         (b (call bf a))
         (y (call cf b)))
    (setf (@gradient y) #(1.0d0))
    (setf (@gradient b) (backward cf (@gradient y)))
    (setf (@gradient a) (backward bf (@gradient b)))
    (setf (@gradient x) (backward af (@gradient a)))
    (format t "~A~%" (@gradient x))))
; #(3.297442541400256d0)
;  => NIL
