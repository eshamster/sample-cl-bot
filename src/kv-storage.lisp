(in-package :cl-user)
(defpackage sample-cl-bot.kv-storage
  (:use :cl
        :anaphora)
  (:export :save-pairs
           :delete-pairs
           :get-pairs-list))
(in-package sample-cl-bot.kv-storage)

(defvar *memory-table* nil)

(defun check-kind (kind)
  (check-type kind keyword))

(defun check-a-pair (pair)
  (check-type pair list)
  (check-type (car pair) keyword)
  (check-type (cdr pair) string))

(defun check-pairs (pairs)
  (dolist (kv pairs)
    (check-a-pair kv)))

(defun check-keys (keys)
  (dolist (key keys)
    (if (listp key)
        (check-a-pair key)
        (check-type key keyword))))

(defun is-same-pairs (target-pairs condition-pairs
                      &optional (keys-to-identify condition-pairs))
  (every #'(lambda (key-or-pair)
             (let ((key (if (listp key-or-pair) (car key-or-pair) key-or-pair)))
               (check-type key keyword)
               (string= (cdr (assoc key target-pairs))
                        (cdr (assoc key condition-pairs)))))
          keys-to-identify))

;; TODO: exclusive lock
(defun delete-pairs (kind condition-pairs
                     &optional (keys-to-identify condition-pairs))
  (check-kind kind)
  (check-pairs condition-pairs)
  (check-keys keys-to-identify)
  (let ((kind-storage (assoc kind *memory-table*)))
    (when kind-storage
      (symbol-macrolet ((storage-place (cdr kind-storage)))
        (let ((before-length (length storage-place)))
          (setf storage-place
                (delete-if #'(lambda (pairs)
                               (is-same-pairs pairs condition-pairs keys-to-identify))
                           storage-place))
          (/= before-length (length storage-place)))))))

;; TODO: exclusive lock
(defun save-pairs (kind pairs &optional (keys-to-identify pairs))
  (check-kind kind)
  (check-pairs pairs)
  (check-keys keys-to-identify)
  (delete-pairs kind pairs keys-to-identify)
  (labels ((get-kind-storage () (assoc kind *memory-table*)))
    (unless (get-kind-storage)
      (push (cons kind nil) *memory-table*))
    (let ((kind-storage (get-kind-storage)))
      (push pairs (cdr kind-storage))))
  *memory-table*)

;; TODO: shared lock
(defun get-pairs-list (kind condition-pairs)
  (check-kind kind)
  (check-pairs condition-pairs)
  (awhen (assoc kind *memory-table*)
    (loop for pairs in (cdr it)
       when (is-same-pairs pairs condition-pairs)
       collect pairs)))
