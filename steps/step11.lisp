(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :array-operations)
  (ql:quickload :rove))

(defpackage :dezero-naive.steps.step11
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
(in-package :dezero-naive.steps.step11)

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


(defgeneric call (function inputs))

(defgeneric forward (function xs))

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
  ((inputs :initform nil :accessor @inputs)
   (outputs :initform nil :accessor @outputs)))

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
                     (x (@inputs func))
                     (y (@outputs func)))
                (setf (@gradient x) (backward func (@gradient y)))
                (when (@creator x)
                  (push (@creator x) funcs))))))


(defmethod call ((func <function>) inputs)
  (let* ((xs (mapcar #'@data inputs))
         (ys (forward func xs))
         (outputs (mapcar (lambda (y) (<variable> (ensure-array y))) ys)))
    (dolist (output outputs)
      (set-creator output func))
    (setf (@inputs func) inputs)
    (setf (@outputs func) outputs)
    outputs))

(defmethod forward ((function <function>) xs)
  (declare (ignore xs))
  (error "not implemented"))

(defmethod backward ((function <function>) &optional x)
  (declare (ignore x))
  (error "not implemented"))


(defclass <add> (<function>) ())

(defun <add> () (make-instance '<add>))

(defun add (xs)
  (call (<add>) xs))

(defmethod forward ((func <add>) xs)
  (list (destructuring-bind (x0 x1) xs
          (aops:vectorize (x0 x1) (+ x0 x1)))))


(let* ((xs (list (<variable> #(2)) (<variable> #(3))))
       (ys (add xs))
       (y (car ys)))
  (format t "~A~%" (@data y)))
; #(5)
;  => NIL
