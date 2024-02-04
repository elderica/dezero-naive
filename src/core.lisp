(in-package :cl-user)
(defpackage :dezero-naive.core
  (:nicknames :dezero-naive.core)
  (:use :cl)
  (:export
   :@a
   :dzvector

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

;;;;;;;;;;;;;;;;;;;; begin <dezero-array> ;;;;;;;;;;;;;;;;;;;;;
(defclass <dezero-array> ()
  ((a :initarg :a
      :accessor @a)))

(defmethod print-object ((a <dezero-array>) stream)
  (print-unreadable-object (a stream :type t :identity nil)
    (format stream
             "~<~W~:>"
             (list (@a a)))))

(defun dezero-array-p (thing)
  (typep thing '<dezero-array>))

(defun make-dezero-array (dimensions &key initial-element initial-contents)
  (declare (optimize (safety 3) (debug 3)))
  (let ((a (cond
             (initial-element
              (make-array dimensions :initial-element initial-element))
             (initial-contents
              (make-array dimensions :initial-contents initial-contents)))))

    (make-instance '<dezero-array> :a a)))

(defun dzvector (&rest elements)
  (declare (optimize (safety 3) (debug 3)))
  (make-dezero-array (length elements) :initial-contents elements))

(defun full-like (array fill-value)
  (declare (optimize (safety 3) (debug 3)))
  (let ((dims (array-dimensions (@a array))))
    (make-dezero-array dims :initial-element fill-value)))

(defun zeros-like (array)
  (declare (optimize (safety 3) (debug 3)))
  (full-like array 0))

(defun ones-like (array)
  (declare (optimize (safety 3) (debug 3)))
  (full-like array 1))

(defgeneric .+. (left right))

(defmethod .+. ((left number) (right number))
  (declare (optimize (safety 3) (debug 3)))
  (+ left right))

(defmethod .+. ((left vector) (right number))
  (declare (optimize (safety 3) (debug 3)))
  (map 'vector (lambda (x) (.+. x right)) left))

(defmethod .+. ((left number) (right vector))
  (declare (optimize (safety 3) (debug 3)))
  (map 'vector (lambda (x) (.+. left x)) right))

(defmethod .+. ((left vector) (right vector))
  (declare (optimize (safety 3) (debug 3)))
  (map 'vector #'.+. left right))

(defmethod .+. ((left <dezero-array>) (right <dezero-array>))
  (declare (optimize (safety 3) (debug 3)))
  (let ((r (.+. (@a left) (@a right))))
    (make-instance '<dezero-array> :a r)))


(defgeneric .*. (left right))

(defmethod .*. ((left number) (right number))
  (* left right))

(defmethod .*. ((left vector) (right number))
  (declare (optimize (safety 3) (debug 3)))
  (map 'vector (lambda (x) (.*. x right)) left))

(defmethod .*. ((left number) (right vector))
  (declare (optimize (safety 3) (debug 3)))
  (map 'vector (lambda (x) (.*. left x)) right))

(defmethod .*. ((left vector) (right vector))
  (declare (optimize (safety 3) (debug 3)))
  (map 'vector #'.*. left right))

(defmethod .*. ((left number) (right <dezero-array>))
  (declare (optimize (safety 3) (debug 3)))
  (let ((r (.*. left (@a right))))
    (make-instance '<dezero-array> :a r)))

(defmethod .*. ((left <dezero-array>) (right <dezero-array>))
  (declare (optimize (safety 3) (debug 3)))
  (let ((r (.*. (@a left) (@a right))))
    (make-instance '<dezero-array> :a r)))
;;;;;;;;;;;;;;;;;;;; end <dezero-array> ;;;;;;;;;;;;;;;;;;;;;


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
  (check-type (@data var) <dezero-array>))

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
                      (gxs (let ((it (apply #'backward func nil gys)))
                             (etypecase it
                               (<dezero-array> (vector it))
                               (vector it)))))

                 (loop for x across (@inputs func)
                       for gx across gxs
                       do (progn
                            (setf (@gradient x)
                                  (if (@gradient x)
                                      (.+. (@gradient x) gx)
                                      gx))

                            (when (@creator x)
                              (add-func (@creator x)))))
                 (unless retain-gradient
                   (loop for y across (@outputs func)
                         do (setf (@gradient (tg:weak-pointer-value y))
                                  nil))))))))
;;;;;;;;;;;;;;;;;;;; end <variable> ;;;;;;;;;;;;;;;;;;;;;


(defun as-array (x)
  (etypecase x
    (number (dzvector x))
    (<dezero-array> x)))


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
         (ys (let ((it (apply #'forward func xs)))
               (etypecase it
                 (<dezero-array> (vector it))
                 (vector it))))
         (outputs (map 'vector (lambda (y)
                                 (<variable> (as-array y)))
                       ys)))
    (when *enable-backpropagation*
      (setf (@generation func)
            (loop for x in inputs maximize (@generation x)))
      (loop for output across outputs
            do (set-creator output func)))
    (setf (@inputs func) (map 'vector #'identity inputs))
    (setf (@outputs func) (map 'vector #'tg:make-weak-pointer outputs))
    (if (> (length outputs) 1)
        outputs
        (aref outputs 0))))
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
    (.+. x0 x1)))

(defmethod backward ((func <add>) &optional (retain-gradient nil) &rest gys)
  (declare (ignore retain-gradient))
  (declare (optimize (safety 3) (debug 3)))
  (let ((gy (first gys)))
    (vector gy gy)))
;;;;;;;;;;;;;;;;;;;; end <add> ;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;; begin <square> ;;;;;;;;;;;;;;;;;;;;;
(defclass <square> (<function>) ())

(defun <square> () (make-instance '<square>))

(defun square (&rest xs)
  (declare (optimize (safety 3) (debug 3)))
  (apply #'call (<square>) xs))

(defmethod forward ((func <square>) &rest xs)
  (declare (optimize (safety 3) (debug 3)))
  (let ((x (first xs)))
    (.*. x x)))

(defmethod backward ((func <square>) &optional (retain-gradient nil) &rest gys)
  (declare (ignore retain-gradient))
  (declare (optimize (safety 3) (debug 3)))
  (let* ((x (@data (elt (@inputs func) 0)))
         (tw (.*. 2 x))
         (gx (map 'vector (lambda (x) (.*. tw x)) gys)))
    gx))
;;;;;;;;;;;;;;;;;;;; end <square> ;;;;;;;;;;;;;;;;;;;;;
