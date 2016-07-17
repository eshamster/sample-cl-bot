(in-package :cl-user)
(defpackage sample-cl-bot-test.utils
  (:use :cl
        :sample-cl-bot.utils
        :prove))
(in-package :sample-cl-bot-test.utils)


(plan 1)

(subtest "Test with-params"
  (let ((params '(("ab" . 1)
                  ("AbCde" . 2)
                  ("ab_cd" . 4)
                  ("ab-cd-ef" . 8))))
    (is (with-params params (ab abcde ab-cd ab_cd-ef)
          (+ ab abcde ab-cd ab_cd-ef))
        15)
    (is (with-params params (ab-cd ab)
          (+ ab ab-cd))
        5)
    (is (with-params params (ab nothing)
          (+ ab (if nothing 100 0)))
        1)))

(finalize)
