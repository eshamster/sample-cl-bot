#|
  This file is a part of sample-cl-bot project.
  Copyright (c) 2016 eshamster (hamgoostar@gmail.com)
|#

#|
  Author: eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage sample-cl-bot-asd
  (:use :cl :asdf))
(in-package :sample-cl-bot-asd)

(defsystem sample-cl-bot
  :version "0.1"
  :author "eshamster"
  :license "MIT"
  :depends-on (:alexandria
               :anaphora
               :clack
               :ningle
               :dexador)
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "kv-storage")
                 (:file "storage")
                 (:file "parser")
                 (:file "sample-cl-bot"))))
  :description "A sample bot by Common Lisp for Slack"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op sample-cl-bot-test))))
