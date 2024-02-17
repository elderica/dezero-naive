(in-package :cl-user)
(defpackage :dezero-naive.core
  (:nicknames :dezero-naive.core)
  (:use :cl)
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
   :square
   ))

(in-package :dezero-naive.core)

(defparameter *enable-backpropagation* t)

(defun full-like (array fill-value)
  (declare (optimize (safety 3) (debug 3)))
  (let ((dims (array-dimensions array)))
    (make-array dims :initial-element fill-value)))

(defun zeros-like (array)
  (declare (optimize (safety 3) (debug 3)))
  (full-like array 0))

(defun ones-like (array)
  (declare (optimize (safety 3) (debug 3)))
  (full-like array 1))

(defgeneric call (function &rest inputs))

(defgeneric forward (function &rest xs))

(defgeneric backward (function-or-variable &optional retain-gradient &rest gys))


;;;;;;;;;;;;;;;;;;;; begin <variable> ;;;;;;;;;;;;;;;;;;;;;
(defclass <variable> ()
  ((data :initarg :data :accessor @data)
   (gradient :initform nil :accessor @gradient)
   (creator :initform nil :accessor @creator)
   (generation :initform 0 :accessor @generation)))

(defmethod initialize-instance :after ((var <variable>) &key)
  (check-type (@data var) array))

(defun <variable> (data)
  (make-instance '<variable> :data data))

(defmethod print-object ((var <variable>) stream)
  (print-unreadable-object (var stream :type t :identity nil)
    (format stream
            "~:@_~<data: ~W ~_gradient: ~W ~_creator: ~W ~_generation: ~W~:>"
            (list (@data var) (@gradient var) (@creator var) (@generation var)))))

(defmethod set-creator ((var <variable>) func)
  (setf (@creator var) func)
  (setf (@generation var) (+ (@generation func) 1)))

(defmethod clear-gradient ((var <variable>))
  (setf (@gradient var) nil))

(defmethod backward ((var <variable>) &optional (retain-gradient nil) &rest gys)
  (declare (optimize (safety 3) (debug 3)))
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
      (loop while funcs
            do (let* ((func (pop funcs))
                      (gys (map 'list
                                (lambda (gy)
                                  (@gradient (tg:weak-pointer-value gy)))
                                (@outputs func)))
                      (gxs (uiop:ensure-list (apply #'backward func nil gys))))
                 (loop for x in (@inputs func)
                       for gx in gxs
                       do (progn
                            (setf (@gradient x)
                                  (uiop:if-let ((agx (@gradient x)))
                                    (aops:vectorize (agx gx)
                                      (+ agx gx))
                                    gx))

                            (when (@creator x)
                              (add-func (@creator x)))))
                 (unless retain-gradient
                   (loop for y in (@outputs func)
                         do (setf (@gradient (tg:weak-pointer-value y))
                                  nil))))))))
;;;;;;;;;;;;;;;;;;;; end <variable> ;;;;;;;;;;;;;;;;;;;;;


(defun ensure-array (x)
  (etypecase x
    (number (vector x))
    (array x)))


;;;;;;;;;;;;;;;;;;;; begin <function> ;;;;;;;;;;;;;;;;;;;;;
(defclass <function> ()
  ((inputs :initform nil :accessor @inputs)
   (outputs :initform nil :accessor @outputs)
   (generation :initform nil :accessor @generation)))

(defmethod print-object ((func <function>) stream)
  (print-unreadable-object (func stream :type t :identity nil)
    (format stream
            "~<generation: ~W~:>"
            (list (@generation func)))))

(defmethod call ((func <function>) &rest inputs)
  (declare (optimize (safety 3) (debug 3)))
  (let* ((xs (map 'list #'@data inputs))
         (ys (uiop:ensure-list (apply #'forward func xs)))
         (outputs (map 'list (lambda (y)
                                 (<variable> (ensure-array y)))
                       ys)))
    (when *enable-backpropagation*
      (setf (@generation func)
            (loop for x in inputs maximize (@generation x)))
      (loop for output in outputs
            do (set-creator output func)))
    (setf (@inputs func) inputs)
    (setf (@outputs func) (map 'list #'tg:make-weak-pointer outputs))
    (if (> (length outputs) 1)
        outputs
        (first outputs))))
;;;;;;;;;;;;;;;;;;;; end <function> ;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;; begin <add> ;;;;;;;;;;;;;;;;;;;;;
(defclass <add> (<function>) ())

(defun <add> () (make-instance '<add>))

(defun add (x0 x1)
  (call (<add>) x0 x1))

(defmethod forward ((func <add>) &rest xs)
  (declare (optimize (safety 3) (debug 3)))
  (let ((x0 (first xs))
        (x1 (second xs)))
     (aops:vectorize (x0 x1)
       (+ x0 x1))))

(defmethod backward ((func <add>) &optional (retain-gradient nil) &rest gys)
  (declare (ignore retain-gradient))
  (declare (optimize (safety 3) (debug 3)))
  (let ((gy (first gys)))
    (list gy gy)))
;;;;;;;;;;;;;;;;;;;; end <add> ;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;; begin <square> ;;;;;;;;;;;;;;;;;;;;;
(defclass <square> (<function>) ())

(defun <square> () (make-instance '<square>))

(defun square (&rest xs)
  (declare (optimize (safety 3) (debug 3)))
  (apply #'call (<square>) xs))

(defgeneric .*. (left right))

(defmethod .*. ((left number) (right vector))
  (declare (optimize (safety 3) (debug 3)))
  (aops:vectorize (right)
    (* left right)))

(defmethod .*. ((left vector) (right vector))
  (declare (optimize (safety 3) (debug 3)))
  (aops:vectorize (left right)
    (* left right)))

(defmethod forward ((func <square>) &rest xs)
  (declare (optimize (safety 3) (debug 3)))
  (let ((x (first xs)))
    (.*. x x)))

(defmethod backward ((func <square>) &optional (retain-gradient nil) &rest gys)
  (declare (ignore retain-gradient))
  (declare (optimize (safety 3) (debug 3)))
  (let* ((x (@data (first (@inputs func))))
         (tw (.*. 2 x))
         (gx (map 'list (lambda (x) (.*. tw x)) gys)))
    gx))
;;;;;;;;;;;;;;;;;;;; end <square> ;;;;;;;;;;;;;;;;;;;;;
