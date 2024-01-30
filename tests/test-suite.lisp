;; Copyright (c) 2024 elderica <1130138+elderica@users.noreply.github.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer
;;     in this position and unchanged.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage :dezero-naive.test
  (:use :cl :rove)
  (:nicknames :dezero-naive.test)
  (:import-from :dezero-naive
   :backward

   :<variable>
   :make-variable
   :@data
   :@gradient

   :<square>
   :square

   :numerical-diff))
(in-package :dezero-naive.test)

(deftest square-test
  (testing "test forword"
    (let* ((x (make-variable (vector 2.0d0)))
           (ys (square (list x)))
           (expected (vector 4.0d0)))
      (ok (equalp (@data (aref ys 0))
                  expected))))
  ;; (testing "test backward"
  ;;   (let* ((x (make-variable (vector 3.0d0)))
  ;;          (y (square x))
  ;;          (expected (vector 6.0d0)))
  ;;     (backward y)
  ;;     (ok (equalp (@gradient x)
  ;;                 expected))))

  ;; (testing "test gradient check"
  ;;   (let* ((x (make-variable (vector (random 1.0d0))))
  ;;          (y (square x)))
  ;;     (backward y)
  ;;     (let* ((num-grad (numerical-diff (make-instance '<square>) x)))
  ;;       (ok (loop for x across (@gradient x)
  ;;                 for y across num-grad
  ;;                 always (<= (/ (abs (- x y))
  ;;                               (abs x))
  ;;                            1d-08))))))
  )
