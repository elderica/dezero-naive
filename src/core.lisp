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
(defpackage :dezero-naive.core
  (:nicknames :dezero-naive.core)
  (:use :cl)
  (:export
   :call
   :forward
   :backward

   :dz-variable
   :dz-variable.data
   :dz-function

   :square
   :exponential

   :numerical-diff

   :compose))
(in-package :dezero-naive.core)

(defgeneric call (callable-object &rest arguments))
(defgeneric forward (self &rest arguments))
(defgeneric backward (self &rest arguments))

(defclass dz-variable ()
  ((data :initarg :data
         :accessor dz-variable.data)))

(defclass dz-function ()
  ((input :initarg :input
          :accessor dz-function.input)
   (output :initarg :output
           :accessor dz-function.output)))

(defmethod call ((func dz-function) &rest arguments)
  (let* ((input (first arguments))
         (x (dz-variable.data input))
         (y (forward func x))
         (output (make-instance 'dz-variable :data y)))
    output))

(defclass square (dz-function) ())

(defmethod forward ((func square) &rest arguments)
  (let* ((x (first arguments)))
    (map 'vector (lambda (i) (* i i)) x)))

(defclass exponential (dz-function) ())

(defmethod forward ((func exponential) &rest arguments)
  (let* ((x (first arguments)))
    (map 'vector #'exp x)))

(defun numerical-diff (func x &optional (eps 1e-4))
  (let* ((x0 (make-instance 'dz-variable
                            :data (map 'vector (lambda (i) (- i eps))
                                       (dz-variable.data x))))
         (x1 (make-instance 'dz-variable
                            :data (map 'vector (lambda (i) (+ i eps))
                                       (dz-variable.data x))))
         (y0 (call func x0))
         (y1 (call func x1)))
    (map 'vector (lambda (i) (/ i (* 2.0 eps)))
         (loop for i1 across (dz-variable.data y1)
               for i0 across (dz-variable.data y0)
               collect (- i1 i0)))))

(defclass composed-function (dz-function)
  ((second :initarg :second)
   (first :initarg :first)))

(defun compose-two (second first)
  (make-instance 'composed-function
                 :second second
                 :first first))

(defun compose (&rest functions)
  (if (null (rest functions))
      (first functions)
      (compose-two (first functions)
                   (apply #'compose (rest functions)))))

(defmethod call ((self composed-function) &rest arguments)
  (let* ((x (first arguments))
         (g (slot-value self 'second))
         (f (slot-value self 'first)))
    (call g (call f x))))
