(in-package :cl-user)
(defpackage sample-cl-bot-test.utils
  (:use :cl
        :sample-cl-bot.utils
        :prove))
(in-package :sample-cl-bot-test.utils)


(plan 4)

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

(defun validate-post-content (result expected-text)
  (ok (jonathan:parse result))
  (let ((json (jonathan:parse result :as :hash-table)))
    (is (gethash "text" json) expected-text)
    (dolist (key '("icon_url" "username"))
      (ok (gethash key json)))))

(subtest "Test make-post-content"
  (validate-post-content (make-post-content "body") "body"))

(subtest "Test make-post-to-mention"
  (subtest "Test normal conditions"
    (validate-post-content (make-post-to-mention "ikaruga" '(("user_name" . "rs2")))
                           "@rs2 ikaruga"))
  (subtest "Test error conditions"
    (is-error (make-post-to-mention "ikaruga" nil)
              'simple-error)
    (is-error (make-post-to-mention "ikaruga" '(("not_include" . "a_user_name")))
              'simple-error)))

(subtest "Test extract-posted-text"
  (labels ((prove-it (text expected)
             (is (extract-posted-text (cons (cons "text" text)
                                            '(("trigger_word" . "trig:"))))
                 expected)))
    (prove-it "trig:body" "body")
    (prove-it "trig:   body  " "body")
    (prove-it " body  " "body")
    (prove-it " body trig: body" "body trig: body")))

(finalize)
