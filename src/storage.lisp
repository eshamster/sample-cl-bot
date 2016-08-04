(in-package :cl-user)
(defpackage sample-cl-bot.storage
  (:use :cl
        :anaphora)
  (:export :save-content
           :delete-content
           :get-content)
  (:import-from :sample-cl-bot.utils
                :with-params)
  (:import-from :sample-cl-bot.kv-storage
                :save-pairs
                :delete-pairs
                :get-pairs-list))
(in-package sample-cl-bot.storage)

(defun make-id (params)
  (with-params params (token channel-id)
    (format nil "token:~A;channel-id:~A" token channel-id)))

(defun check-identifier-types (kind key)
  (check-type kind keyword)
  (check-type key string))

(defun save-content (kind params key value)
  (check-identifier-types kind key)
  (check-type value string)
  (save-pairs kind
              `((:id . ,(make-id params))
                (:key . ,key)
                (:value . ,value))
              '(:id :key)))

(defun get-content (kind params key)
  (check-identifier-types kind key)
  (mapcar #'(lambda (pairs)
              (cons (cdr (assoc :key pairs))
                    (cdr (assoc :value pairs))))
          (get-pairs-list kind `((:id . ,(make-id params))
                                 (:key . ,key)))))

(defun delete-content (kind params key)
  (check-identifier-types kind key)
  (delete-pairs kind `((:id . ,(make-id params))
                       (:key . ,key))))
