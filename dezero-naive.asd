(defsystem "dezero-naive"
  :description "dezero in Common Lisp"
  :version "0.0.1"
  :author "elderica <1130138+elderica@users.noreply.github.com>"
  :license "BSD 2-Clause"
  :depends-on (:trivial-garbage
               :array-operations)
  :pathname "src/"
  :components ((:file "core"))
  :in-order-to ((test-op (test-op "dezero-naive.test"))))
