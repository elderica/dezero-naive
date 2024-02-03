(ql:quickload :dezero-naive.test)
;(setf rove:*enable-colors* nil)
(asdf:test-system :dezero-naive.test)
(uiop:quit (length (rove/core/stats:all-failed-assertions rove/core/stats:*stats*)))
