(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))

(asdf:load-system :dezero-naive :force t)
(asdf:load-system :dezero-naive.test :force t)

(asdf:test-system :dezero-naive.test)

(sb-cover:report (asdf:system-relative-pathname :dezero-naive "../cover/"))

(declaim (optimize (sb-cover:store-coverage-data 0)))
