(in-package :cl-user)
(defpackage sample-cl-bot
  (:use :cl
        :alexandria
        :anaphora)
  (:import-from :sample-cl-bot.utils
                :with-params
                :make-post-content
                :extract-posted-text)
  (:import-from :sample-cl-bot.parser
                :parse-input)
  (:export *app*))
(in-package :sample-cl-bot)

(defvar *app* (make-instance 'ningle:<app>))

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

;; --- routing --- ;;

(setf (ningle:route *app* "/" :method :POST)
      #'(lambda (params)
          (dex:post (get-incoming-hook-url)
                    :content (parse-input (extract-posted-text params) params)
                    :headers '(("content-type" . "application/json")))))
