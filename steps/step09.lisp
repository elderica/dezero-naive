(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :array-operations))

(defpackage :dezero-naive.steps.step09
  (:use :common-lisp)
  (:shadow :exp))
(in-package :dezero-naive.steps.step09)

(defun full-like (array fill-value)
  (let ((dims (array-dimensions array)))
    (make-array dims :initial-element fill-value)))

(defun zeros-like (array)
  (full-like array 0))

(defun ones-like (array)
  (full-like array 1))

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


(defun ensure-array (x)
  (if (numberp x)
      (vector x)
      x))


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


(let* ((x (<variable> #(0.5d0)))
       (y (square (exp (square x)))))
    (backward y)
  (format t "~A~%" (@gradient x)))
; #(3.297442541400256d0)
;  => NIL
