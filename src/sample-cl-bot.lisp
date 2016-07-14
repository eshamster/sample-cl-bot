(in-package :cl-user)
(defpackage sample-cl-bot
  (:use :cl
        :alexandria)
  (:export *app*))
(in-package :sample-cl-bot)

(defvar *app* (make-instance 'ningle:<app>))

;; --- parameter modifier --- ;;

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

(defun make-post-content (text)
  (jonathan:to-json
   (list :|text| text 
         :|icon_url| "http://www.lisperati.com/lisplogo_alien_128.png" 
         :|username| "Lisp Alien")))

(defun extract-posted-text (params)
  (with-params params (text trigger-word)
    (trim-trigger-word trigger-word text)))

;; --- settings manager --- ;;

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

;; --- parser --- ;;

(defun separate-command (text)
  (let ((splitted (ppcre:split "\\s" text :limit 2)))
    (cons (make-keyword (string-upcase (car splitted)))
          (cdr splitted))))

(defun parse-input (text params)
  (let* ((separated (separate-command text))
         (command (car separated))
         (body (cadr separated)))
    (case command
      (:hello (with-params params (user-name)
                (make-post-content (format nil "Hello ~A!!" user-name))))
      (:echo (make-post-content body))
      (t (make-post-content (format nil "I don't know the command '~A' :cow2:" command))))))

;; --- routing --- ;;

(setf (ningle:route *app* "/" :method :POST)
      #'(lambda (params)
          (dex:post (get-incoming-hook-url)
                    :content (parse-input (extract-posted-text params) params)
                    :headers '(("content-type" . "application/json")))))
