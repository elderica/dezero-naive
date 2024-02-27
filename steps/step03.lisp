(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :array-operations))

(defpackage :dezero-naive.steps.step03
  (:use :common-lisp))
(in-package :dezero-naive.steps.step03)

(defgeneric call (function input))

(defgeneric forward (function x))

(defclass <variable> ()
  ((data :initarg :data :accessor @data)))

(defun <variable> (data)
  (make-instance '<variable> :data data))

(defclass <function> ()
  ())

(defun <function> ())

(defmethod call ((func <function>) input)
  (let* ((x (@data input))
         (y (forward func x))
         (output (<variable> y)))
    output))

(defmethod forward ((function <function>) x)
  (error "not implemented"))

(defclass <square> (<function>)
  ())

(defun <square> ()
  (make-instance '<square>))

(defmethod forward ((func <square>) x)
  (aops:vectorize (x)
    (* x x)))

(defclass <exp> (<function>)
  ())

(defun <exp> ()
  (make-instance '<exp>))

(defmethod forward ((func <exp>) x)
  (aops:vectorize (x)
    (exp x)))

(let ((af (<square>))
      (bf (<exp>))
      (cf (<square>)))
  (let* ((x (<variable> #(0.5d0)))
         (a (call af x))
         (b (call bf a))
         (y (call cf b)))
    (format t "~A~%" (@data y))))
; #(1.648721270700128d0)
;  => NIL
