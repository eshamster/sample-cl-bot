(in-package :cl-user)
(defpackage sample-cl-bot.storage
  (:use :cl
        :anaphora)
  (:export :save-content
           :delete-content
           :get-content)
  (:import-from :sample-cl-bot.kv-storage
                :save-pairs
                :delete-pairs
                :get-pairs-list))
(in-package sample-cl-bot.storage)

(defun check-identifier-types (kind id key)
  (check-type kind keyword)
  (check-type id string)
  (check-type key string))

(defun is-same-content (pair id key)
  (and (string= (cdr (assoc :id pair)) id) 
       (string= (cdr (assoc :key pair)) key)))

(defun find-same-content (pairs id key)
  (find-if #'(lambda (pair)
               (is-same-content pair id key))
           pairs))

(defun save-content (kind id key value)
  (check-identifier-types kind id key)
  (check-type value string)
  (save-pairs kind
              `((:id . ,id)
                (:key . ,key)
                (:value . ,value))
              '(:id :key)))

(defun get-content (kind id key)
  (check-identifier-types kind id key)
  (cdr (assoc :value
              (car (get-pairs-list kind `((:id . ,id)
                                          (:key . ,key)))))))

(defun delete-content (kind id key)
  (check-identifier-types kind id key)
  (delete-pairs kind `((:id . ,id)
                       (:key . ,key))))
