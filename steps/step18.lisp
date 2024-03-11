(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :array-operations)
  (ql:quickload :trivial-garbage)
  (ql:quickload :rove))

(defpackage :dezero-naive.steps.step18
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
(in-package :dezero-naive.steps.step18)

(defparameter *enable-backpropagation* t)
(defparameter *retain-gradient* nil)

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
(defgeneric backward (function-or-variable &optional gys))

(defclass <variable> ()
  ((data :initarg :data :accessor @data)
   (gradient :initform nil :accessor @gradient)
   (creator :initform nil :accessor @creator)
   (generation :initform 0 :accessor @generation)))

(defun <variable> (data)
  (make-instance '<variable> :data data))

(defmethod initialize-instance :after ((var <variable>) &key)
  (check-type (@data var) array))

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

(defmethod backward ((var <variable>) &optional gys)
  (declare (ignore gys))
  (unless (@gradient var)
    (setf (@gradient var) (ones-like (@data var))))
  (let (funcs seen-set)
    (flet ((add-func (f)
             (unless (member f seen-set)
               (push f funcs)
               (pushnew f seen-set)
               (setf funcs (sort funcs #'> :key #'@generation)))))
      (add-func (@creator var))
      (loop :while funcs :do
        (let* ((func (pop funcs))
               (gys (mapcar (lambda (gy)
                              (@gradient (tg:weak-pointer-value gy)))
                            (@outputs func)))
               (gxs (uiop:ensure-list (apply #'backward func gys))))
          (loop :for x :in (@inputs func)
                :for gx :in gxs
                :do (with-accessors ((x-grad @gradient)) x
                      (setf x-grad (if x-grad
                                       (aops:vectorize (x-grad gx)
                                         (+ x-grad gx))
                                       gx))
                      (when (@creator x)
                        (add-func (@creator x)))))
          (unless *retain-gradient*
            (dolist (y (@outputs func))
              (setf (@gradient (tg:weak-pointer-value y))
                    nil))))))))

(defmethod call ((func <function>) &rest inputs)
  (let* ((xs (mapcar #'@data inputs))
         (ys (uiop:ensure-list (apply #'forward func xs)))
         (outputs (mapcar (lambda (y) (<variable> (ensure-array y))) ys)))
    (when *enable-backpropagation*
      (setf (@generation func)
            (reduce #'max inputs :key #'@generation))
      (dolist (output outputs)
        (set-creator output func)))
    (setf (@inputs func) inputs)
    (setf (@outputs func) (mapcar #'tg:make-weak-pointer outputs))
    (if (= (length outputs) 1)
        (car outputs)
        outputs)))

(defmethod forward ((function <function>) &rest xs)
  (declare (ignore xs))
  (error "not implemented"))

(defmethod backward ((function <function>) &optional gys)
  (declare (ignore gys))
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

(let ((*retain-gradient* nil))
  (let* ((x0 (<variable> #(1.0)))
         (x1 (<variable> #(1.0)))
         (u (add x0 x1))
         (y (add x0 u)))
    (backward y)
    (format t "~A ~A~%~A ~A~%"
            (@gradient y) (@gradient u)
            (@gradient x0) (@gradient x1))))

(let ((*retain-gradient* t))
  (let* ((x0 (<variable> #(1.0)))
         (x1 (<variable> #(1.0)))
         (u (add x0 x1))
         (y (add x0 u)))
    (backward y)
    (format t "~A ~A~%~A ~A~%"
            (@gradient y) (@gradient u)
            (@gradient x0) (@gradient x1))))
;; (time
;;  (dotimes (i 1000)
;;    (let ((x (<variable> (aops:generate
;;                          (lambda () (random 1.0d0))
;;                          100000))))
;;      (square (square (square x))))))
;; #+sbcl
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (require 'sb-sprof))
;; #+sbcl
;; (sb-sprof:with-profiling (:reset t :mode :alloc :report :flat :loop nil)
;;   (let ((x (<variable> (aops:generate
;;                          (lambda () (random 1.0d0))
;;                          100000))))
;;      (square (square (square x)))))

(defun numerical-diff (f x &optional (eps 1.0d-4))
  (let* ((x (@data x))
         (x0 (<variable> (aops:vectorize (x) (- x eps))))
         (x1 (<variable> (aops:vectorize (x) (+ x eps))))
         (y0 (@data (call f x0)))
         (y1 (@data (call f x1))))
    (aops:vectorize (y1 y0)
      (/ (- y1 y0)
         (* 2 eps)))))

(defpackage :dezero-naive.steps.step18.test
  (:use :common-lisp :rove)
  (:import-from :dezero-naive.steps.step18
   :backward
   :<variable>
   :@data
   :@gradient
   :square
   :<square>
   :numerical-diff))
(in-package :dezero-naive.steps.step18.test)

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

(run-suite :dezero-naive.steps.step18.test)
