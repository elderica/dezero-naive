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

   :<variable>
   :make-variable
   :@data
   :@gradient

   :<square>
   :square
;;   :exponential
;;   :exponentialf

   :numerical-diff
   ))
;;   :compose))
(in-package :dezero-naive.core)

(defgeneric call (func inputs))
(defgeneric forward (func xs))
(defgeneric backward (func-or-var &optional gy))

(defclass <variable> ()
  ((data :initarg :data
         :accessor @data)
   (gradient :initform nil
             :accessor @gradient)
   (creator :initform nil
            :accessor @creator)))

(defmethod initialize-instance :after ((var <variable>) &key)
  (check-type (slot-value var 'data) (array * *)))

(defun make-variable (data)
  (make-instance '<variable> :data data))

(defmethod print-object ((var <variable>) stream)
  (print-unreadable-object (var stream :type t :identity nil)
    (format stream
            "~:@_~<data: ~W ~_gradient: ~W ~_creator: ~W~:>"
            (list (@data var) (@gradient var) (@creator var)))))

(defmethod set-creator ((var <variable>) func)
  (setf (@creator var) func))

(defmethod backward ((var <variable>) &optional gy)
  (declare (ignore gy))
  (unless (@gradient var)
    (setf (@gradient var)
          (make-array (array-dimensions (@data var))
                      :initial-element 1.0d0)))
  (loop with funcs = (list (@creator var))
        until (null funcs)
        do (let* ((func (pop funcs))
                  (x (@inputs func))
                  (y (@outputs func)))
             (setf (@gradient x)
                   (backward func (@gradient y)))
             (when (@creator x)
               (push (@creator x) funcs)))))

(defun as-array (x)
  (typecase x
    ((array * *) x)
    (t (vector x))))

(defclass <function> ()
  ((inputs :initform nil
           :accessor @inputs)
   (outputs :initform nil
           :accessor @outputs)))

(defmethod print-object ((func <function>) stream)
  (print-unreadable-object (func stream :type t :identity nil)
    (format stream
            "~:@_~<inputs: ~W ~:_outputs: ~W~:>"
            (list (@inputs func) (@outputs func)))))

(defmethod call ((func <function>) inputs)
  (let* ((xs (map 'list #'@data inputs))
         (ys (forward func xs))
         (outputs (map 'vector (lambda (y)
                                 (make-variable (as-array y)))
                       ys)))
    (loop for output across outputs do (set-creator output func))
    (setf (@inputs func) inputs)
    (setf (@outputs func) outputs)
    outputs))

(defclass <square> (<function>) ())

 (defun square (xs)
   (call (make-instance '<square>) xs))

 (defmethod forward ((func <square>) xs)
   (map 'vector (lambda (i) (* i i)) (first xs)))

;; (defmethod backward ((func <square>) &optional gy)
;;   (let* ((x (@data (@inputs func)))
;;          (gx (map 'vector (lambda (i0 i1) (* i0 i1 2.0d0)) x gy)))
;;     gx))

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
  (let* ((x0 (make-variable (map 'vector (lambda (i) (- i eps)) (@data x))))
         (x1 (make-variable (map 'vector (lambda (i) (+ i eps)) (@data x))))
         (y0 (call func x0))
         (y1 (call func x1)))
    (map 'vector (lambda (i1 i0) (/ (- i1 i0) (* 2.0d0 eps)))
         (@data y1)
         (@data y0))))

;; (defclass <composed-function> (dz-function)
;;   ((second :initarg :second)
;;    (first :initarg :first)))

;; (defun compose-two (second first)
;;   (make-instance '<composed-function>
;;                  :second second
;;                  :first first))

;; (defun compose (&rest functions)
;;   (if (null (rest functions))
;;       (first functions)
;;       (compose-two (first functions)
;;                    (apply #'compose (rest functions)))))

;; (defmethod call ((self <composed-function>) input)
;;   (let ((g (slot-value self 'second))
;;         (f (slot-value self 'first)))
;;     (call g (call f input))))
