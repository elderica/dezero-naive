(in-package :common-lisp-user)

(defpackage :dezero-naive.steps.step02
  (:use :common-lisp))
(in-package :dezero-naive.steps.step02)

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

(let* ((x (<variable> #(10)))
       (f (<square>))
       (y (call f x)))
  (format t "~A~%" (type-of y))
  (format t "~A~%" (@data y)))
; <VARIABLE>
; #(100)
;  => NIL
