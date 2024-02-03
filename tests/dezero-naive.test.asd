(defsystem "dezero-naive.test"
  :licence "BSD 2-Clauses"
  :depends-on (:rove
               :dezero-naive)
  :components ((:file "test-suite"))
  :perform (test-op (op c) (uiop:symbol-call :rove :run-suite :dezero-naive.test)))
