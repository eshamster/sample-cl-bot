(in-package :cl-user)
(defpackage sample-cl-bot.utils
  (:use :cl)
  (:export :with-params
           :make-post-content
           :make-post-to-mention
           :extract-posted-text
           :find-all-nodes))
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

(defun make-post-to-mention (text params)
  (with-params params (user-name)
    (if user-name
        (make-post-content (format nil "@~A ~A" user-name text))
        (error "The params doesn't include a user_name field"))))

(defun trim-trigger-word (trigger-word text)
  (string-trim " "
               (ppcre:regex-replace (format nil "^~A" trigger-word)
                                    (string-trim "\"" text)
                                    "")))

(defun extract-posted-text (params)
  (with-params params (text trigger-word)
    (trim-trigger-word trigger-word text)))

(defun find-all-nodes (predicate tree)
  (labels ((loop-list (func list)
              "This can do with a not nil-ended list"
              (when list
                (let ((head (car list))
                      (rest (cdr list)))
                  (funcall func head)
                  (if (listp rest)
                      (loop-list func rest)
                      (funcall func rest)))))
           (rec (tree)
             (let (result)
               (when (funcall predicate tree)
                 (push tree result))
               (when (listp tree)
                 (loop-list
                    #'(lambda (node)
                        (setf result (append (rec node) result)))
                    tree))
               result)))
    (rec tree)))
