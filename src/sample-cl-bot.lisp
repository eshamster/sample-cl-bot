(in-package :cl-user)
(defpackage sample-cl-bot
  (:use :cl)
  (:export *app*))
(in-package :sample-cl-bot)

(defvar *app* (make-instance 'ningle:<app>))

(defun format-key (symbol)
  (ppcre:regex-replace "-"
                       (symbol-name symbol)
                       "_"))

(defmacro with-params (params bindings &body body)
  `(let ,(mapcar #'(lambda (name)
                     `(,name (cdr (assoc (format-key ',name) ,params :test #'string-equal))))
                 bindings)
     ,@body))

(defun trim-trigger-word (trigger-word text)
  (string-trim " "
               (ppcre:regex-replace (format nil "^~A" trigger-word)
                                    (string-trim "\"" text)
                                    "")))

;; TODO: cache hash
(defun get-setting-hash ()
  (let* ((path (make-pathname :directory (pathname-directory
                                          (asdf:system-source-file :sample-cl-bot))
                              :name "settings"
                              :type "json"))
         (read-list (with-open-file (stream path)
                      (loop for line = (read-line stream nil)
                         while line
                         collect line))))
    (jonathan:parse (apply #'concatenate (cons 'string read-list))
                    :as :hash-table)))

(defun get-incoming-hook-url ()
  (gethash "incoming_hook" (get-setting-hash)))

(defun make-post-content (text)
  (jonathan:to-json
   (list :|text| text 
         :|icon_url| "http://www.lisperati.com/lisplogo_alien_128.png" 
         :|username| "Lisp Alien")))

(defun extract-posted-text (params)
  (with-params params (text trigger-word)
    (trim-trigger-word trigger-word text)))

(setf (ningle:route *app* "/" :method :POST)
      #'(lambda (params)
          (with-params params (user-name)
            (make-post-content
             (format nil "Hello, ~A!! I'm a Lisp Alian!!~%You said \"~A\""
                     user-name
                     (extract-posted-text params))))))
