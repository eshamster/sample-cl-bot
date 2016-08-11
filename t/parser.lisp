(in-package :cl-user)
(defpackage sample-cl-bot-test.parser
  (:use :cl
        :sample-cl-bot.parser
        :prove))
(in-package :sample-cl-bot-test.parser)

;; --- utils --- ;;

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

(import 'sample-cl-bot-test.kv-storage::with-test-storage)

(defmacro with-fresh-env (&body body)
  `(let ((sample-cl-bot.parser::*continuity-table* (make-hash-table :test 'equalp)))
     (with-test-storage
       ,@body)))

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

(defun exec-simple-test-set (&rest rest)
  (assert (evenp (length rest)))
  (when rest
    (exec-simple-test (car rest) (cadr rest))
    (apply #'exec-simple-test-set (cddr rest))))

;; --- tests --- ;;

(plan 3)

(with-fresh-env
  (subtest "Test simple commands"
    (subtest "Test hello"
      (exec-simple-test "hello" (list "hello" *user*))
      (exec-simple-test "hEllO" (list "hello" *user*)))
    (subtest "Test echo"
      (exec-simple-test "echo say something" '("^say something$")))
    (subtest "Test a not-exist command"
      (exec-simple-test "not-exist test" '("not-exist" "don't know")))))

(subtest "Test commands about key-value store"
  (let ((not-remembered "have not remembered"))
    (with-fresh-env
      (exec-simple-test-set
       "get rs2" (list not-remembered "rs2")
       "remember" '("input" "key" "value")
       "rs2" '("What")
       "ikaruga" '("rs2" "ikaruga")
       "get rs2" '("ikaruga")
       ;; update
       "remember rs2 = black and white" '("rs2" "black and white")
       "get rs2" '("black and white")
       ;; add a next item
       "remember abc" '("What")
       "xyz" '("abc" "xyz")
       "get rs2" '("black and white")
       "get abc" '("xyz")
       ;; delete
       "forget" '("What")
       "forget not-exist-item" (list not-remembered)
       "forget abc" '("forgetted" "abc")
       "get abc" (list not-remembered)))))

(with-fresh-env
  (subtest "Test weather command"
    (let ((valid-checker '("[0-9]{4}.[0-9]{2}.[0-9]{2}" "http"))
          (invalid-checker '("don't know" "see" "http")))
      (exec-simple-test-set
       "weather not-exist-city" invalid-checker
       "weather 東京" valid-checker
       "wf 東京" valid-checker
       ;; check if the remembered word is accepted
       "wf tk" invalid-checker
       "remember tk=東京" '("tk" "東京")
       "wf tk" valid-checker))))

(finalize)
