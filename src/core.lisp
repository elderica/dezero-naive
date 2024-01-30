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
   :dz-variable.gradient
   :dz-variable.creator

   :dz-function

   :square
   :squaref
   :exponential
   :exponentialf

   :numerical-diff

   :compose))
(in-package :dezero-naive.core)

(defgeneric call (callable-object input))
(defgeneric forward (self &rest arguments))
(defgeneric backward (self &rest arguments))

(defclass dz-variable ()
  ((data :initarg :data
         :accessor dz-variable.data)
   (gradient :initform nil
             :accessor dz-variable.gradient)
   (creator :initform nil
            :accessor dz-variable.creator)))

(defmethod initialize-instance :after ((var dz-variable) &key)
  (check-type (slot-value var 'data) (array * *)))

(defmethod set-creator ((var dz-variable) func)
  (setf (dz-variable.creator var) func))

(defmethod backward ((var dz-variable) &rest arguments)
  (declare (ignore arguments))
  (unless (dz-variable.gradient var)
    (setf (dz-variable.gradient var)
          (make-array (array-dimensions (dz-variable.data var))
                      :initial-element 1.0d0)))
  (loop with funcs = (list (dz-variable.creator var))
        until (null funcs)
        do (let* ((func (pop funcs))
                  (x (dz-function.input func))
                  (y (dz-function.output func)))
             (setf (dz-variable.gradient x)
                   (backward func (dz-variable.gradient y)))
             (when (dz-variable.creator x)
               (push (dz-variable.creator x) funcs)))))

(defclass dz-function ()
  ((input :initarg :input
          :initform nil
          :accessor dz-function.input)
   (output :initarg :output
           :initform nil
           :accessor dz-function.output)))

(defmethod call ((func dz-function) input)
  (let* ((x (dz-variable.data input))
         (y (forward func x))
         (output (make-instance 'dz-variable :data y)))
    (set-creator output func)
    (setf (dz-function.input func) input)
    (setf (dz-function.output func) output)
    output))

(defclass square (dz-function) ())

(defun squaref (x)
  (call (make-instance 'square) x))

(defmethod forward ((func square) x)
  (map 'vector (lambda (i) (* i i)) x))

(defmethod backward ((func square) &optional gy)
  (let* ((x (dz-variable.data (dz-function.input func)))
         (gx (map 'vector (lambda (i0 i1) (* i0 i1 2.0d0)) x gy)))
    gx))

;; (defclass exponential (dz-function) ())

;; (defun exponentialf (x)
;;   (call (make-instance 'exponential) x))

;; (defmethod forward ((func exponential) &rest arguments)
;;   (let* ((x (first arguments)))
;;     (map 'vector #'exp x)))

;; (defmethod backward ((func exponential) &rest arguments)
;;   (let* ((gy (first arguments))
;;          (x (dz-variable.data (dz-function.input func)))
;;          (gx (map 'vector (lambda (i0 i1) (* (exp i0) i1)) x gy)))
;;     gx))

(defun numerical-diff (func x &optional (eps 1d-4))
  (let* ((x0 (make-instance 'dz-variable
                            :data (map 'vector (lambda (i) (- i eps))
                                       (dz-variable.data x))))
         (x1 (make-instance 'dz-variable
                            :data (map 'vector (lambda (i) (+ i eps))
                                       (dz-variable.data x))))
         (y0 (call func x0))
         (y1 (call func x1)))
    (map 'vector (lambda (i1 i0) (/ (- i1 i0) (* 2.0d0 eps)))
         (dz-variable.data y1)
         (dz-variable.data y0))))

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

(defmethod call ((self composed-function) input)
  (let ((g (slot-value self 'second))
        (f (slot-value self 'first)))
    (call g (call f input))))
