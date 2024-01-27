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
