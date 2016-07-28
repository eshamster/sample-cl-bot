(in-package :cl-user)
(defpackage sample-cl-bot.storage
  (:use :cl
        :anaphora)
  (:export :save-content
           :delete-content
           :get-content))
(in-package sample-cl-bot.storage)

(defvar *memory-table* nil)

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

;; TODO: exclusive lock
(defun save-content (kind id key value)
  (check-identifier-types kind id key)
  (check-type value string)
  (labels ((get-kind-storage () (assoc kind *memory-table*)))
    (unless (get-kind-storage)
      (push (cons kind nil) *memory-table*))
    (let ((kind-storage (get-kind-storage)))
      (aif (find-same-content (cdr kind-storage) id key)
           (setf (cdr (assoc :value it)) value)
           (push `((:id . ,id)
                   (:key . ,key)
                   (:value . ,value))
                 (cdr kind-storage)))))
  *memory-table*)

;; TODO: shared lock
(defun get-content (kind id key)
  (check-identifier-types kind id key)
  (awhen (find-same-content (cdr (assoc kind *memory-table*)) id key)
    (cdr (assoc :value it))))

;; TODO: exclusive lock
(defun delete-content (kind id key)
  (check-identifier-types kind id key)
  (let ((kind-storage (assoc kind *memory-table*)))
    (awhen (find-same-content (cdr kind-storage) id key)
      (progn (setf (cdr kind-storage)
                   (delete-if #'(lambda (pair)
                                  (is-same-content pair id key))
                              (cdr kind-storage)))
             it))))
