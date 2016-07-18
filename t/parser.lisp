(in-package :cl-user)
(defpackage sample-cl-bot-test.parser
  (:use :cl
        :sample-cl-bot.parser
        :prove))
(in-package :sample-cl-bot-test.parser)

(defun validate-json (json-str)
  (let ((json-hash (jonathan:parse json-str :as :hash-table)))
    (when (every #'(lambda (target)
                     (multiple-value-bind (value exists) (gethash target json-hash)
                       (cond ((null exists)
                              (format *error-output* "The key ~D is not exist" target)
                              nil)
                             ((not (stringp value))
                              (format *error-output* "The value ~D is not string (key=~D)"
                                      value target))
                             (t t))))
                 '("text" "icon_url" "username"))
      json-hash)))

(defmacro with-fresh-env (&body body)
  `(let ((sample-cl-bot.parser::*continuity-table* (make-hash-table)))
     ,@body))

(defvar *trigger* "alien:")
(defvar *user* "test-user")

(defun exec-simple-test (text expected-words)
  (let* ((ret (parse-input text
                           `(("text" . ,(format nil "~D ~D" *trigger* text))
                             ("trigger_word" . ,*trigger*)
                             ("user_name" . ,*user*)
                             ("token" . "dummy-token")
                             ("team_id" . "dummy-team-id")
                             ("user_id" . "dummy-user-id"))))
         (json-hash (validate-json ret))
         (ret-text (gethash "text" json-hash))) 
    (ok (every #'(lambda (word)
                   (if (ppcre:scan (ppcre:create-scanner word :case-insensitive-mode t)
                                   ret-text)
                       t
                       (format *error-output* "\"~D\" is not included in \"~D\"" word ret-text)))
               expected-words))
    (format t "~8@A<output-text> ~A" "" ret-text)))

(plan 1)

(with-fresh-env
  (subtest "Test simple commands"
    (subtest "Test hello"
      (exec-simple-test "hello" (list "hello" *user*))
      (exec-simple-test "hEllO" (list "hello" *user*)))
    (subtest "Test echo"
      (exec-simple-test "echo say something" '("^say something$")))
    (subtest "Test a not-exist command"
      (exec-simple-test "not-exist test" '("not-exist" "don't know")))))

(finalize)
