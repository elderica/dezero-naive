(require 'asdf)
(asdf:load-asd "dezero-naive.asd")
(ql:quickload :dezero-naive)

(in-package :dezero-naive)

(let* ((x (make-instance 'dz-variable :data (vector 2 2)))
       (f (make-instance 'square))
       (y (call f x)))
  (format t "~A~%"
          (dz-variable.data y)))

(let* ((x (make-instance 'dz-variable :data (vector 0.5)))
       (f (make-instance 'exponential))
       (y (call f x)))
  (format t "~A~%"
          (dz-variable.data y)))

(let* ((af (make-instance 'square))
       (bf (make-instance 'exponential))
       (cf (make-instance 'square))
       (x (make-instance 'dz-variable :data (vector 0.5)))
       (a (call af x))
       (b (call bf a))
       (y (call cf b)))
  (format t "~A~%"
          (dz-variable.data y)))

(let* ((f (make-instance 'square))
       (x (make-instance 'dz-variable :data (vector 2.0)))
       (dy (numerical-diff f x)))
  (format t "~A~%" dy))

(let* ((cf (compose (make-instance 'square)
                    (make-instance 'exponential)
                    (make-instance 'square)))
       (x (make-instance 'dz-variable :data (vector 0.5)))
       (dy (numerical-diff cf x)))
  (format t "~A~%"
          dy))

(let* ((af (make-instance 'square))
       (bf (make-instance 'exponential))
       (cf (make-instance 'square))

       (x (make-instance 'dz-variable :data (vector 0.5)))
       (a (call af x))
       (b (call bf a))
       (y (call cf b)))
  (format t "data: ~A~%" (dz-variable.data y))
  (setf (dz-variable.gradient y) (vector 1.0))
  (setf (dz-variable.gradient b)
        (backward cf (dz-variable.gradient y)))
  (setf (dz-variable.gradient a)
        (backward bf (dz-variable.gradient b)))
  (setf (dz-variable.gradient x)
        (backward af (dz-variable.gradient a)))
  (format t "gradient: ~A~%"
          (dz-variable.gradient x)))