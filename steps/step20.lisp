(in-package :common-lisp-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :array-operations)
  (ql:quickload :trivial-garbage)
  (ql:quickload :rove))
(defpackage :dezero-naive.steps.step20
  (:use :common-lisp)
  (:shadow :exp)
  (:export
   :backward
   :<variable>
   :@data
   :@gradient
   :square
   :<square>
   :multiply
   :numerica-diff))
(in-package :dezero-naive.steps.step20)

(defparameter *enable-backpropagation* t)
(defparameter *retain-gradient* nil)

(defun zeros-like (array)
  "return an array of zeros with the same dimensions and element type as a given array"
  (aops:zeros* (aops:element-type array) (aops:dims array)))

(defun ones-like (array)
  "return an array of ones with the same dimensions and element type as a given array"
  (aops:ones* (aops:element-type array) (aops:dims array)))

(defun ensure-array (x)
  (if (arrayp x) x (vector x)))

(defgeneric call (function &rest inputs))
(defgeneric forward (function &rest xs))
(defgeneric backward (function-or-variable &optional gys))

(defclass <variable> ()
  ((data :initarg :data :accessor @data)
   (name :initarg :name :initform nil :accessor @name)
   (gradient :initform nil :accessor @gradient)
   (creator :initform nil :accessor @creator)
   (generation :initform 0 :accessor @generation)))

(defun <variable> (data &optional name)
  (make-instance '<variable> :data data :name name))

(defmethod initialize-instance :after ((var <variable>) &key)
  (check-type (@data var) array))

(defmethod print-object ((var <variable>) stream)
  (print-unreadable-object (var stream :type t :identity nil)
    (format stream
            "~:@_~<data: ~W ~_name: ~W ~_gradient: ~W ~_creator: ~W ~_generation: ~W~:>"
            (list (@data var) (@name var)
                  (@gradient var) (@creator var) (@generation var)))))

(defmethod shape ((var <variable>))
  "list of array dimensions"
  (aops:dims (@data var)))

(defmethod ndim ((var <variable>))
  "number of array dimenstions"
  (aops:rank (@data var)))

(defmethod size ((var <variable>))
  "number of elements in the array"
  (aops:size (@data var)))

(defmethod dtype ((var <variable>))
  "data type of array's elements"
  (aops:element-type (@data var)))

(defmethod len ((var <variable>))
  "number of array's first dimention"
  (aops:dim (@data var) 0))

(defclass <function> ()
  ((inputs :initform nil :accessor @inputs)
   (outputs :initform nil :accessor @outputs)
   (generation :initform nil :accessor @generation)))

(defmethod print-object ((func <function>) stream)
  (print-unreadable-object (func stream :type t :identity nil)
    (format stream
            "~<generation: ~W~:>"
            (list (@generation func)))))

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

(defclass <add> (<function>) ())

(defun <add> () (make-instance '<add>))

(defun add (&rest xs) (apply #'call (<add>) xs))

(defmethod forward ((func <add>) &rest xs)
  (list (destructuring-bind (x0 x1) xs
          (aops:vectorize (x0 x1) (+ x0 x1)))))

(defmethod backward ((func <add>) &optional gy)
  (list gy gy))

(defclass <square> (<function>) ())

(defun <square> () (make-instance '<square>))

(defun square (x) (call (<square>) x))

(defmethod forward ((func <square>) &rest xs)
  (let ((x (first xs)))
    (aops:vectorize (x)
      (* x x))))

(defmethod backward ((func <square>) &optional gy)
  (let* ((x (@data (first (@inputs func))))
         (gx (aops:vectorize (x gy) (* x gy 2.0d0))))
    gx))

(defclass <multiply> (<function>) ())

(defun <multiply> () (make-instance '<multiply>))

(defun multiply (x0 x1) (call (<multiply>) x0 x1))

(defmethod forward ((func <multiply>) &rest xs)
  (destructuring-bind (x0 x1) xs
    (aops:vectorize (x0 x1) (* x0 x1))))

(defmethod backward ((func <multiply>) &optional gy)
  (let ((x0 (@data (first (@inputs func))))
        (x1 (@data (second (@inputs func)))))
    (list (aops:vectorize (gy x1) (* gy x1))
          (aops:vectorize (gy x0) (* gy x0)))))

(let ((a (<variable> #(3.0)))
      (b (<variable> #(2.0)))
      (c (<variable> #(1.0))))
  (let ((y (add (multiply a b) c)))
    (backward y)
    (format t "~A~%" y)
    (format t "~A~%" (@gradient a))
    (format t "~A~%" (@gradient b))))
; #<<VARIABLE> data: #(7.0)
;              name: NIL
;              gradient: NIL
;              creator: #<<ADD> generation: 1>
;              generation: 2>
; #(2.0)
; #(3.0)
;  => NIL

(defun numerical-diff (f x &optional (eps 1.0d-4))
  (let* ((x (@data x))
         (x0 (<variable> (aops:vectorize (x) (- x eps))))
         (x1 (<variable> (aops:vectorize (x) (+ x eps))))
         (y0 (@data (call f x0)))
         (y1 (@data (call f x1))))
    (aops:vectorize (y1 y0)
      (/ (- y1 y0)
         (* 2 eps)))))

(defpackage :dezero-naive.steps.step20.test
  (:use :common-lisp :rove)
  (:import-from :dezero-naive.steps.step20
   :backward
   :<variable>
   :@data
   :@gradient
   :square
   :<square>
   :add
   :multiply
   :numerical-diff))
(in-package :dezero-naive.steps.step20.test)

(defun all-close (x y)
  (>= 1d-08 (aops:vectorize-reduce #'max (x y)
              (/ (abs (- x y)) (abs x)))))

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

(deftest multiply-test
  (testing "test forward"
    (let ((a (<variable> #(3.0)))
      (b (<variable> #(2.0)))
      (c (<variable> #(1.0))))
  (let ((y (add (multiply a b) c)))
    (backward y)
    (ok (equalp (@data y) #(7.0)))
    (ok (equalp (@gradient a) #(2.0)))
    (ok (equalp (@gradient b) #(3.0)))))))


(run-suite :dezero-naive.steps.step20.test)
