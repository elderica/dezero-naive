(require 'asdf)
(asdf:load-asd "dezero-naive.asd")
(ql:quickload :dezero-naive)

(in-package :dezero-naive)

(let* ((x (make-instance 'dz-variable :data (vector 2 2)))
       (f (make-instance 'square))
       (y (call f x)))
  (format t "~A~%"
	  (dz-variable.data y)))
