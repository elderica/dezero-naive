(in-package :common-lisp-user)

(defpackage :dezero-naive.steps.step01
  (:use :common-lisp))
(in-package :dezero-naive.steps.step01)

(defclass <variable> ()
  ((data :initarg :data :accessor @data)))

(defun <variable> (data)
  (make-instance '<variable> :data data))

(let* ((data #(1.0))
       (x (<variable> data)))
  (format t "~A~%" (@data x))
  (setf (@data x) #(2.0))
  (format t "~A~%" (@data x)))
; #(1.0)
; #(2.0)
;  => NIL
