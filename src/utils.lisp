(in-package :cl-user)
(defpackage sample-cl-bot.utils
  (:use :cl)
  (:export :with-params
           :make-post-content
           :extract-posted-text))
(in-package :sample-cl-bot.utils)

;; --- parameter modifier --- ;;

(defun format-key (key)
  (ppcre:regex-replace-all "-"
                           (if (symbolp key) (symbol-name key) key)
                           "_"))

(defmacro with-params (params bindings &body body)
  `(let ,(mapcar #'(lambda (name)
                     `(,name (cdr (assoc ',name ,params
                                         :test #'(lambda (x y)
                                                   (string-equal (format-key x) (format-key y)))))))
                 bindings)
     ,@body))

(defun make-post-content (text)
  (jonathan:to-json
   (list :|text| text 
         :|icon_url| "http://www.lisperati.com/lisplogo_alien_128.png" 
         :|username| "Lisp Alien")))

(defun trim-trigger-word (trigger-word text)
  (string-trim " "
               (ppcre:regex-replace (format nil "^~A" trigger-word)
                                    (string-trim "\"" text)
                                    "")))

(defun extract-posted-text (params)
  (with-params params (text trigger-word)
    (trim-trigger-word trigger-word text)))
