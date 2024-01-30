(ql:quickload :dezero-naive)

(in-package :dezero-naive)

(let* ((xs (list (make-variable (vector 2.0d0))
                 (make-variable (vector 3.0d0))))
       (f (make-instance '<add>))
       (ys (call f xs))
       (y (aref ys 0)))
  (format t "~A~%" (@data y)))
