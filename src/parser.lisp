(in-package :cl-user)
(defpackage sample-cl-bot.parser
  (:use :cl
        :alexandria
        :anaphora)
  (:import-from :sample-cl-bot.utils
                :with-params
                :make-post-content
                :make-post-to-mention)
  (:import-from :sample-cl-bot.storage
                :save-content
                :get-content
                :delete-content)
  (:export :parse-input))
(in-package sample-cl-bot.parser)

;; --- parser utilities --- ;;

(defvar *continuity-table* (make-hash-table :test 'equalp))

(defun make-params-hash (params)
  (with-params params (token channel-id user-id)
    (format nil "token:~A-channel:~A-user:~A" token channel-id user-id)))

(defun separate-command (text)
  (let ((splitted (ppcre:split "\\s" text :limit 2)))
    (cons (make-keyword (string-upcase (car splitted)))
          (cdr splitted))))

;; --- remember command --- ;;

(defun register-pair-and-make-post (key value params)
  (let ((key (string-trim " " key))
        (value (string-trim " " value)))
    (save-content :remember 
                  params
                  key
                  value)
    (make-post-to-mention
     (format nil "I remembered that '~A' is '~A'!" key value)
     params)))

(defun make-asking-key-fn ()
  (lambda (text params)
    (parse-remember-command text params)))

(defun is-empty-string (str)
  (or (null str)
      (ppcre:scan "^\\s*$" str)))

(defun make-asking-value-fn (key)
  (lambda (text params)
    (if (is-empty-string text)
        (values (make-post-to-mention
                 (format nil "What is '~A'?" key)
                 params)
                (make-asking-value-fn key))
        (register-pair-and-make-post key text params))))

(defun parse-remember-command (text params)
  (cond ((is-empty-string text) ;; without key and without value
         (values (make-post-to-mention
                  "Please input a key name or a key-value pair (<key>=<value>)"
                  params)
                 (make-asking-key-fn)))
        (t (let* ((pair (ppcre:split "\\s*=\\s*" text :limit 2))
                  (key (car pair))
                  (value (cadr pair)))
             (if value
                 (register-pair-and-make-post key value params)
                 (values (make-post-to-mention
                          (format nil "What is '~A'?" key)
                          params)
                         (make-asking-value-fn key)))))))

;; --- get command --- ;;
(defun parse-get-command (text params)
  (make-post-content
   (if (not (is-empty-string text))
       (let ((key (string-trim " " text)))
         (aif (get-content :remember params key)
              (format nil "It's '~A'!" (cdar it))
              (format nil "I have not remembered '~A'..." key)))
       (format nil "What do you want to know? Please re-input with some key."))))

;; --- forget command --- ;;
(defun parse-forget-command (text params)
  (make-post-content
   (if (not (is-empty-string text))
       (let ((key (string-trim " " text)))
         (aif (delete-content :remember params key)
              (format nil "I forgetted '~A'..." key)
              (format nil "I have not remembered '~A'..." key)))
       (format nil "What do you want me to forget? Please re-input with some key."))) )

;; --- number game --- ;;

(defun make-number-game (answer rest-chance)
  (lambda (text params)
    (labels ((make-message (str)
               (make-post-to-mention str params))
             (make-lose-message ()
               (make-message (format nil "You Lose!! (answer is ~D)" answer)))
             (make-false-return (is-small)
               (let ((next-rest-chance (1- rest-chance)))
                 (if (> next-rest-chance 0)
                     (values (make-message (format nil "It's ~A... (~D more chance[s])"
                                                   (if is-small "small" "large")
                                                   next-rest-chance))
                             (make-number-game answer next-rest-chance)) 
                     (make-lose-message)))))
      (if (not (or (string-equal "quit" text)
                   (string-equal "exit" text))) 
          (let ((number (parse-integer text :junk-allowed t)))
            (if number
                (cond ((= number answer) (make-message "That's right!! You Win!!"))
                      ((< number answer) (make-false-return t))
                      ((> number answer) (make-false-return nil))
                      (t (error "Illegal number-game state")))
                (values (make-message "A number game is not ended. Please input a number.")
                        (make-number-game answer rest-chance))))
          (make-lose-message)))))

(defun start-number-game (text params)
  (declare (ignore text))
  (with-params params (user-name)
    (let ((max 32)
          (chance 3))
      (values (make-post-content
               (format nil "Hi, ~A. Let's start a number game!~%I think of a number. What's it? (1-~D)~%You can answer ~D times."
                       user-name max chance))
              (make-number-game (1+ (random max)) chance)))))

;; --- standard parsers --- ;;

(defun parse-start-command (text params)
  (let* ((separated (separate-command text))
         (command (car separated))
         (body (cadr separated)))
    (case command
      (:number-game (start-number-game body params))
      (t (make-post-content (format nil "I don't know '~A' :cow2:" command))))))

(defun parse-command (text params)
  (let* ((separated (separate-command text))
         (command (car separated))
         (body (cadr separated)))
    (case command
      (:hello (with-params params (user-name)
                (make-post-content (format nil "Hello ~A!!" user-name))))
      (:echo (make-post-content body))
      (:forget (parse-forget-command body params))
      (:get (parse-get-command body params))
      (:remember (parse-remember-command body params))
      (:start (parse-start-command body params))
      (t (make-post-content (format nil "I don't know the command '~A' :cow2:" command))))))

(defun parse-input (text params)
  (slet (gethash (make-params-hash params) *continuity-table*)
    (multiple-value-bind (content continuity)
        (if it (funcall it text params) (parse-command text params))
      (setf it continuity)
      content)))
