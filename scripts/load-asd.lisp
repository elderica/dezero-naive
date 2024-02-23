(cl:require 'asdf)
(asdf:load-asd (merge-pathnames
                #p"dezero-naive.asd"))
(asdf:load-asd (merge-pathnames
                #p"dezero-naive.test.asd"))
