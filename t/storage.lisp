(in-package :cl-user)
(defpackage sample-cl-bot-test.storage
  (:use :cl
        :sample-cl-bot.storage
        :prove))
(in-package :sample-cl-bot-test.storage)

(defmacro with-test-storage (&body body)
  `(let ((sample-cl-bot.storage::*memory-table* nil))
     ,@body))

(plan 2)

(with-test-storage
  (subtest "Test normal operations"
    (ok (not (get-content :kind-a "id-a" "key-a")))
    (save-content :kind-a "id-a" "key-a" "aaa")
    (save-content :kind-a "id-a" "key-b" "aab")
    (save-content :kind-a "id-b" "key-a" "abb")
    (save-content :kind-b "id-a" "key-a" "baa")
    (is (get-content :kind-a "id-a" "key-a")
        "aaa")
    (save-content :kind-a "id-a" "key-a" "aaa-updated")
    (is (get-content :kind-a "id-a" "key-a")
        "aaa-updated")
    (ok (delete-content :kind-a "id-a" "key-a"))
    (ok (not (get-content :kind-a "id-a" "key-a")))
    (ok (not (delete-content :kind-a "id-a" "key-a")))))

(with-test-storage
  (subtest "Test type-error"
    (let ((valid-kind :some-kind)
          (valid-id "some-id")
          (valid-key "some-key")
          (valid-value "some-value"))
      (subtest "save-content"
        (save-content valid-kind valid-id valid-key valid-value) ;; don't throw error
        (is-error (save-content 'invalid-kind valid-id valid-key valid-value)
                  'type-error)
        (is-error (save-content valid-kind :invalid-id valid-key valid-value)
                  'type-error)
        (is-error (save-content valid-kind valid-id :invalid-key valid-value)
                  'type-error)
        (is-error (save-content valid-kind valid-id valid-key :invalid-value)
                  'type-error))
      (subtest "get-content"
        (get-content valid-kind valid-id valid-key) ;; don't throw error
        (is-error (get-content 'abc valid-id valid-key)
                  'type-error)
        (is-error (get-content valid-kind :invalid-id valid-key)
                  'type-error)
        (is-error (get-content valid-kind valid-id :invalid-key)
                  'type-error))
      (subtest "delete-content"
        (delete-content valid-kind valid-id valid-key) ;; don't throw error
        (is-error (delete-content 'abc valid-id valid-key)
                  'type-error)
        (is-error (delete-content valid-kind :invalid-id valid-key)
                  'type-error)
        (is-error (delete-content valid-kind valid-id :invalid-key)
                  'type-error)))))

(finalize)
