#|
  This file is a part of sample-cl-bot project.
  Copyright (c) 2016 eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage sample-cl-bot-test-asd
  (:use :cl :asdf))
(in-package :sample-cl-bot-test-asd)

(defsystem sample-cl-bot-test
  :author "eshamster"
  :license ""
  :depends-on (:sample-cl-bot
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "sample-cl-bot"))))
  :description "Test system for sample-cl-bot"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
