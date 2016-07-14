(in-package :cl-user)
(defpackage sample-cl-bot
  (:use :cl
        :alexandria
        :anaphora)
  (:export *app*))
(in-package :sample-cl-bot)

(defvar *app* (make-instance 'ningle:<app>))

;; --- parameter modifier --- ;;

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

(defun make-post-content (text)
  (jonathan:to-json
   (list :|text| text 
         :|icon_url| "http://www.lisperati.com/lisplogo_alien_128.png" 
         :|username| "Lisp Alien")))

(defun extract-posted-text (params)
  (with-params params (text trigger-word)
    (trim-trigger-word trigger-word text)))

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

;; --- parser --- ;;

(defvar *continuity-table* (make-hash-table :test 'equalp))

(defun make-params-hash (params)
  (with-params params (token channel-id user-id)
    (format nil "token:~A-channel:~A-user:~A" token channel-id user-id)))

(defun separate-command (text)
  (let ((splitted (ppcre:split "\\s" text :limit 2)))
    (cons (make-keyword (string-upcase (car splitted)))
          (cdr splitted))))

(defun make-number-game (answer rest-chance)
  (lambda (text params)
    (labels ((make-message (str)
               (with-params params (user-name)
                 (make-post-content (format nil "@~A ~A" user-name str))))
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
      (:start (parse-start-command body params))
      (t (make-post-content (format nil "I don't know the command '~A' :cow2:" command))))))

(defun parse-input (text params)
  (slet (gethash (make-params-hash params) *continuity-table*)
    (multiple-value-bind (content continuity)
        (if it (funcall it text params) (parse-command text params))
      (setf it continuity)
      content)))

;; --- routing --- ;;

(setf (ningle:route *app* "/" :method :POST)
      #'(lambda (params)
          (dex:post (get-incoming-hook-url)
                    :content (parse-input (extract-posted-text params) params)
                    :headers '(("content-type" . "application/json")))))
