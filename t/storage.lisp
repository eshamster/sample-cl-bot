(in-package :cl-user)
(defpackage sample-cl-bot-test.storage
  (:use :cl
        :sample-cl-bot.storage
        :prove))
(in-package :sample-cl-bot-test.storage)

(import 'sample-cl-bot-test.kv-storage::with-test-storage)

(plan 2)

(defvar *valid-params* '(("token" . "some-token")
                         ("channel_id" . "some-channel-id")
                         ("user_name" . "some-user-name")))

(defvar *valid-params-b* '(("token" . "some-token-b")
                           ("channel_id" . "some-channel-id-b")
                           ("user_name" . "some-user-name-b")))

(with-test-storage
  (subtest "Test normal operations"
    (ok (not (get-content :kind-a *valid-params* "key-a")))
    (save-content :kind-a *valid-params* "key-a" "aaa")
    (save-content :kind-a *valid-params* "key-b" "aab")
    (save-content :kind-a *valid-params-b* "key-a" "abb")
    (save-content :kind-b *valid-params* "key-a" "baa")
    (is (get-content :kind-a *valid-params* "key-a")
        '(("key-a" . "aaa")))
    (save-content :kind-a *valid-params* "key-a" "aaa-updated")
    (is (get-content :kind-a *valid-params* "key-a")
        '(("key-a" . "aaa-updated")))
    (ok (delete-content :kind-a *valid-params* "key-a"))
    (ok (not (get-content :kind-a *valid-params* "key-a")))
    (ok (not (delete-content :kind-a *valid-params* "key-a")))))

(with-test-storage
  (subtest "Test type-error"
    (let ((valid-kind :some-kind)
          (valid-key "some-key")
          (valid-value "some-value"))
      (subtest "save-content"
        (save-content valid-kind *valid-params* valid-key valid-value) ;; don't throw error
        (is-error (save-content 'invalid-kind *valid-params* valid-key valid-value)
                  'type-error)
        (is-error (save-content valid-kind :invalid-id valid-key valid-value)
                  'type-error)
        (is-error (save-content valid-kind *valid-params* :invalid-key valid-value)
                  'type-error)
        (is-error (save-content valid-kind *valid-params* valid-key :invalid-value)
                  'type-error))
      (subtest "get-content"
        (get-content valid-kind *valid-params* valid-key) ;; don't throw error
        (is-error (get-content 'abc *valid-params* valid-key)
                  'type-error)
        (is-error (get-content valid-kind :invalid-id valid-key)
                  'type-error)
        (is-error (get-content valid-kind *valid-params* :invalid-key)
                  'type-error))
      (subtest "delete-content"
        (delete-content valid-kind *valid-params* valid-key) ;; don't throw error
        (is-error (delete-content 'abc *valid-params* valid-key)
                  'type-error)
        (is-error (delete-content valid-kind :invalid-id valid-key)
                  'type-error)
        (is-error (delete-content valid-kind *valid-params* :invalid-key)
                  'type-error)))))

(finalize)
