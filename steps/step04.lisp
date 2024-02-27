(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :array-operations))

(defpackage :dezero-naive.steps.step04
  (:use :common-lisp))
(in-package :dezero-naive.steps.step04)

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

(defun numerical-diff (f x &optional (eps 1.0d-4))
  (let* ((x (@data x))
         (x0 (<variable> (aops:vectorize (x) (- x eps))))
         (x1 (<variable> (aops:vectorize (x) (+ x eps))))
         (y0 (@data (call f x0)))
         (y1 (@data (call f x1))))
    (aops:vectorize (y1 y0)
      (/ (- y1 y0)
         (* 2 eps)))))

(let* ((f (<square>))
       (x (<variable> #(2.0d0)))
       (dy (numerical-diff f x)))
  (format t "~A~%" dy))
; #(4.000000000004d0)
;  => NIL

(defclass <composed-function> (<function>)
  ((g :initarg :g)
   (f :initarg :f)))

(defmethod call ((func <composed-function>) x)
  (call (slot-value func 'g)
        (call (slot-value func 'f) x)))

(defun compose (g f)
  (make-instance '<composed-function> :g g :f f))

(defparameter f
  (let ((af (<square>))
        (bf (<exp>))
        (cf (<square>)))
    (compose cf (compose bf af))))

(let* ((x (<variable> #(0.5d0)))
       (dy (numerical-diff f x)))
  (format t "~A~%" dy))
; #(3.2974426293330694d0)
;  => NIL
