(in-package :common-lisp-user)
(defpackage :dezero-naive.core
  (:nicknames :dezero :dz)
  (:use :common-lisp)
  (:shadow :exp)
  (:export
   :call
   :backward

   :<variable>
   :@data
   :@gradient
   :clear-gradient

   :@generation

   :<add>
   :add
   :<square>
   :square))
(in-package :dezero-naive.core)

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
