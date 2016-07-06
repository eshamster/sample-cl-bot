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

(setf (ningle:route *app* "/" :method :POST)
      #'(lambda (params)
          (with-params params (text trigger-word)
            (format nil "~A" (trim-trigger-word trigger-word text)))))
