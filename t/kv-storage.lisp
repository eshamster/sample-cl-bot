(in-package :cl-user)
(defpackage sample-cl-bot-test.kv-storage
  (:use :cl
        :sample-cl-bot.kv-storage
        :prove))
(in-package :sample-cl-bot-test.kv-storage)

(defmacro with-test-storage (&body body)
  `(let ((sample-cl-bot.kv-storage::*memory-table* nil))
     ,@body))

(plan 2)

(with-test-storage
  (subtest "Test normal operations"
    (labels ((test-get-result (pairs-lst key expected-value-lst)
               (ok (= (length pairs-lst) (length expected-value-lst)))
               (dolist (expected-value expected-value-lst)
                 (ok (find-if #'(lambda (pairs)
                                  (string= (cdr (assoc key pairs))
                                           expected-value))
                              pairs-lst)))))
      (subtest "Test save-pairs"
        (ok (not (get-pairs-list :kind-a '((:ab . "ab-1")))))
        (save-pairs :kind-a '((:ab . "ab1") (:cd . "cd1")))
        (save-pairs :kind-a '((:ab . "ab1") (:cd . "cd2")))
        ;; This should overwrite the previous one
        (save-pairs :kind-a '((:ab . "ab1") (:cd . "cd2")))
        ;; This should not be gotten by the next get-pairs-lst (because of different the value of :ab)
        (save-pairs :kind-a '((:ab . "ab2") (:cd . "cd1")))
        ;; This should not be gotten by the next get-pairs-lst (because of different kind)
        (save-pairs :kind-b '((:ab . "ab1") (:cd . "cd1")))
        (test-get-result (get-pairs-list :kind-a '((:ab . "ab1")))
                         :cd
                         '("cd1" "cd2")))
      (subtest "Test updating by save-pairs"
        (labels ((test-for-updating (expected-value)
                   (test-get-result (get-pairs-list :kind-a '((:ab . "ab-update")))
                                    :cd
                                    (list expected-value))))
          (save-pairs :kind-a '((:ab . "ab-update") (:cd . "cd")))
          (test-for-updating "cd")
          (save-pairs :kind-a '((:ab . "ab-update") (:cd . "cd-updated")) '(:ab))
          (test-for-updating "cd-updated")))
      (subtest "Test delete-pairs"
        (labels ((find-target (search-value)
                   (get-pairs-list :kind-a `((:ab . ,search-value)))))
          (save-pairs :kind-a '((:ab . "delete") (:cd . "cd")))
          (save-pairs :kind-a '((:ab . "not-delete") (:cd . "cd")))
          (ok (find-target "delete"))
          (ok (find-target "not-delete"))
          (ok (delete-pairs :kind-a '((:ab . "delete"))))
          (ok (not (delete-pairs :kind-a '((:ab . "not-found")))))
          (ok (not (find-target "delete")))
          (ok (find-target "not-delete")))))))

(with-test-storage
  (subtest "Test type-error"
    (let ((valid-kind :some-kind)
          (valid-pairs '((:ab . "cd") (:cd . "def")))
          (valid-keys '(:ab :cd :ef)))
      (subtest "save-pairs"
        (save-pairs valid-kind valid-pairs)
        (save-pairs valid-kind valid-pairs valid-keys)
        (is-error (save-pairs 'invalid-kind valid-pairs)
                  'type-error)
        (is-error (save-pairs valid-kind '((:ab . "cd") (invalid . "def")))
                  'type-error)
        (is-error (save-pairs valid-kind '((:ab . invalid) (:cd . "def")))
                  'type-error)
        (is-error (save-pairs valid-kind valid-pairs '(:ab "invalid"))
                  'type-error))
      (subtest "get-pairs"
        (get-pairs-list valid-kind valid-pairs)
        (is-error (get-pairs-list 'invalid-kind valid-pairs)
                  'type-error)
        (is-error (get-pairs-list valid-kind '((:ab . "cd") (invalid . "def")))
                  'type-error)
        (is-error (get-pairs-list valid-kind '((:ab . invalid) (:cd . "def")))
                  'type-error))
      (subtest "delete-pairs"
        (delete-pairs valid-kind valid-pairs)
        (delete-pairs valid-kind valid-pairs valid-keys)
        (is-error (delete-pairs 'invalid-kind valid-pairs)
                  'type-error)
        (is-error (delete-pairs valid-kind '((:ab . "cd") (invalid . "def")))
                  'type-error)
        (is-error (delete-pairs valid-kind '((:ab . invalid) (:cd . "def")))
                  'type-error)
        (is-error (delete-pairs valid-kind valid-pairs '(:ab "invalid"))
                  'type-error)))))

(finalize)
