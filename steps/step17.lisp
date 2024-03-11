(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :array-operations)
  (ql:quickload :trivial-garbage)
  (ql:quickload :rove))

(defpackage :dezero-naive.steps.step17
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
(in-package :dezero-naive.steps.step17)

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


(defgeneric call (function &rest inputs))

(defgeneric forward (function &rest xs))

(defgeneric backward (function-or-variable &optional gy))


(defclass <variable> ()
  ((data :initarg :data :accessor @data)
   (gradient :initform nil :accessor @gradient)
   (creator :initform nil :accessor @creator)
   (generation :initform 0 :accessor @generation)))

(defmethod initialize-instance :after ((var <variable>) &key)
  (check-type (@data var) array))

(defun <variable> (data)
  (make-instance '<variable> :data data))

(defclass <function> ()
  ((inputs :initform nil :accessor @inputs)
   (outputs :initform nil :accessor @outputs)
   (generation :initform nil :accessor @generation)))

(defun <function> ())


(defmethod set-creator ((var <variable>) func)
  (setf (@creator var) func)
  (setf (@generation var) (+ (@generation func) 1)))

(defmethod clear-gradient ((var <variable>))
  (setf (@gradient var) nil))

(defmethod backward ((var <variable>) &optional gy)
  (declare (ignore gy))

  (unless (@gradient var)
    (setf (@gradient var) (ones-like (@data var))))

  (let (funcs seen-set)
    (flet ((add-func (f)
             (unless (member f seen-set)
               (push f funcs)
               (pushnew f seen-set)
               (setf funcs (sort funcs #'> :key #'@generation)))))
      (add-func (@creator var))
      (loop :while funcs
            :do (let* ((func (pop funcs))
                       (gys (mapcar (lambda (gy)
                                      (@gradient (tg:weak-pointer-value gy)))
                                    (@outputs func)))
                       (gxs (uiop:ensure-list (apply #'backward func gys))))
                (loop :for x :in (@inputs func)
                      :for gx :in gxs
                      :do (progn
                            (setf (@gradient x)
                                  (uiop:if-let ((agx (@gradient x)))
                                    (aops:vectorize (agx gx)
                                      (+ agx gx))
                                    gx))
                            (when (@creator x)
                              (add-func (@creator x))))))))))


(defmethod call ((func <function>) &rest inputs)
  (let* ((xs (mapcar #'@data inputs))
         (ys (uiop:ensure-list (apply #'forward func xs)))
         (outputs (mapcar (lambda (y) (<variable> (ensure-array y))) ys)))
    (setf (@generation func)
          (loop :for x :in inputs maximize (@generation x)))
    (dolist (output outputs)
      (set-creator output func))
    (setf (@inputs func) inputs)
    (setf (@outputs func) (mapcar #'tg:make-weak-pointer outputs))
    (if (= (length outputs) 1)
        (car outputs)
        outputs)))

(defmethod forward ((function <function>) &rest xs)
  (declare (ignore xs))
  (error "not implemented"))

(defmethod backward ((function <function>) &optional x)
  (declare (ignore x))
  (error "not implemented"))


(defclass <add> (<function>) ())

(defun <add> () (make-instance '<add>))

(defun add (&rest xs)
  (apply #'call (<add>) xs))

(defmethod forward ((func <add>) &rest xs)
  (list (destructuring-bind (x0 x1) xs
          (aops:vectorize (x0 x1) (+ x0 x1)))))

(defmethod backward ((func <add>) &optional gy)
  (list gy gy))


(defclass <square> (<function>) ())

(defun <square> () (make-instance '<square>))

(defun square (x)
  (call (<square>) x))

(defmethod forward ((func <square>) &rest xs)
  (let ((x (first xs)))
    (aops:vectorize (x)
      (* x x))))

(defmethod backward ((func <square>) &optional gy)
  (let* ((x (@data (first (@inputs func))))
         (gx (aops:vectorize (x gy) (* x gy 2.0d0))))
    gx))


(time
 (dotimes (i 1000)
   (let ((x (<variable> (aops:generate
                         (lambda () (random 1.0d0))
                         10000))))
     (square (square (square x))))))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-sprof))
#+sbcl
(sb-sprof:with-profiling (:reset t :mode :alloc :report :flat :loop t)
  (let ((x (<variable> (aops:generate
                         (lambda () (random 1.0d0))
                         100000))))
     (square (square (square x)))))
