(in-package :common-lisp)
(defpackage :dezero-naive.test
  (:use :common-lisp :rove)
  (:import-from :dezero-naive.core
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
(in-package :dezero-naive.test)

(deftest square-test
    (testing "test forword single variable"
             (let* ((x (<variable> (vector 2.0d0)))
                    (y (square x))
                    (expected (vector 4.0d0)))
               (ok (equalp (@data y)
                           expected))))

  (testing "test backward"
    (let* ((x (<variable> (vector 3.0d0)))
           (y (square x))
           (expected (vector 6.0d0)))
      (backward y)
      (ok (equalp (@gradient x)
                  expected)))))

(deftest add-test
  (testing "test add"
    (let* ((x0 (<variable> (vector 7.0d0)))
           (x1 (<variable> (vector 5.0d0)))
           (y (add x0 x1))
           (r (@data y))
           (expected (vector 12.0d0)))
      (ok (equalp r expected)))))

(deftest add-square-test
  (testing "test backward add and square"
    (let* ((x (<variable> (vector 2.0d0)))
           (y (<variable> (vector 3.0d0)))
           (z (add (square x) (square y))))
      (backward z)
      (ok (equalp (@data z)
                  (vector 13.0d0)))
      (ok (equalp (@gradient x)
                  (vector 4.0d0)))
      (ok (equalp (@gradient y)
                  (vector 6.0d0))))))

(deftest use-same-variable
  (testing "same variable"
    (let* ((x (<variable> (vector 3.d0)))
           (y (add x x)))
      (backward y)
      (ok (equalp (@gradient x)
                  (vector 2.d0)))))

  (testing "same variable again"
    (let* ((x (<variable> (vector 3.d0)))
           (y (add (add x x) x)))
      (backward y)
      (ok (equalp (@gradient x)
                  (vector 3.d0))))

  (testing "backward twice"
    (let* ((x (<variable> (vector 3.0d0)))
           (y (add x x)))
      (backward y)
      (ok (equalp (@gradient x)
                  (vector 2.d0)))
      (clear-gradient x)
      (let ((y (add (add x x) x)))
        (backward y)
        (ok (equalp (@gradient x)
                    (vector 3.d0))))))))

(deftest step16
  (testing "step 16"
    (let* ((x (<variable> (vector 2.0)))
           (>A> (<square>))
           (a (call >A> x))
           (>B> (<square>))
           (b (call >B> a))
           (>C> (<square>))
           (c (call >C> a))
           (>D> (<add>))
           (y (call >D> b c)))
      (backward y)
      (ok (= (@generation x) 0))
      (ok (= (@generation >A>) 0))
      (ok (= (@generation a) 1))
      (ok (= (@generation >B>) 1))
      (ok (= (@generation b) 2))
      (ok (= (@generation >C>) 1))
      (ok (= (@generation c) 2))
      (ok (= (@generation >D>) 2))
      (ok (= (@generation y) 3))
      (ok (equalp (@data y)
                  (vector 32.d0)))
      (ok (equalp (@gradient x)
                  (vector 64.d0))))))
