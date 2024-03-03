(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :array-operations))

(defpackage :dezero-naive.steps.step07
  (:use :common-lisp))
(in-package :dezero-naive.steps.step07)

(defgeneric call (function input))

(defgeneric forward (function x))

(defgeneric backward (func-or-var &optional gy))

(defclass <variable> ()
  ((data :initarg :data :accessor @data)
   (gradient :initform nil :accessor @gradient)
   (creator :initform nil :accessor @creator)))

(defun <variable> (data)
  (make-instance '<variable> :data data))

(defclass <function> ()
  ((input :accessor @input)
   (output :accessor @output)))

(defun <function> ())

(defmethod set-creator ((var <variable>) func)
  (setf (@creator var) func))

(defmethod backward ((var <variable>) &optional gy)
  (declare (ignore gy))
  (let ((func (@creator var)))
    (when func
      (let ((x (@input func)))
        (setf (@gradient x) (backward func (@gradient var)))
        (backward x)))))

(defmethod call ((func <function>) input)
  (let* ((x (@data input))
         (y (forward func x))
         (output (<variable> y)))
    (set-creator output func)
    (setf (@input func) input)
    (setf (@output func) output)
    output))

(defmethod forward ((function <function>) x)
  (error "not implemented"))

(defmethod backward ((function <function>) &optional x)
  (declare (ignore x))
  (error "not implemented"))

(defclass <square> (<function>)
  ())

(defun <square> ()
  (make-instance '<square>))

(defmethod forward ((func <square>) x)
  (aops:vectorize (x)
    (* x x)))

(defmethod backward ((func <square>) &optional gy)
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

(defmethod backward ((func <exp>) &optional gy)
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
    (backward y)
    (format t "~A~%" (@gradient x))))
