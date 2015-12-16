;;; -*- lexical-binding: t -*-

(require 'url)
(require 'potato-channel)

(defgroup potato nil
  "Potato client implementation for Emacs"
  :prefix 'potato
  :group 'applications)

(defface potato-default
  ()
  "Default face for potato buffers."
  :group 'potato)

(defface potato-message-from
  '((((class color))
     :foreground "#00b000"
     :inherit potato-default)
    (t
     :inherit potato-default))
  "Face used to display the 'from' part of a message."
  :group 'potato)

(defcustom potato-api-token ""
  "API token for the user"
  :type 'string
  :group 'potato)

(defcustom potato-url "http://potato.dhsdevelopments.com/"
  "The main url for the Potato instance"
  :type 'string
  :group 'potato)

(defcustom potato-channel-id ""
  "Default channel to connect to"
  :type 'string
  :group 'potato)

(defun potato--string-match-start (string key)
  (and (>= (length string) (length key))
       (string= (subseq string 0 (length key)) key)))

(defun potato--assoc-with-check (tag alist)
  (let ((value (assoc tag alist)))
    (unless value
      (error "No value for tag: %s" tag))
    (cdr value)))

(defun potato--url-handler (status buffer callback)
  (let ((error-status (getf status :error)))
    (if error-status
        (progn
          (message "Got error: %S" status)
          (signal (car error-status) (cdr error-status)))
      ;; ELSE: No error
      (progn
        (goto-char (point-min))
        (search-forward "\n\n")
        (let* ((content (buffer-substring (point) (point-max)))
               (decoded-content (decode-coding-string content 'utf-8))
               (data (json-read-from-string decoded-content)))
          (with-current-buffer buffer
            (funcall callback data)))))))

(defun potato--url-retrieve (url method callback)
  (let ((url-request-method method)
        (url-request-extra-headers `(("API-Token" . ,potato--active-api-token)))
        (url (format "%s%sapi/1.0%s"
                     potato--active-url
                     (if (eql (aref potato--active-url (1- (length potato--active-url))) ?/) "" "/")
                     url)))
    (let ((buffer (current-buffer)))
      (url-retrieve url (lambda (status)
                          (potato--url-handler status buffer callback))
                    nil t))))

(defun potato--input (str)
  (let ((url-request-data (json-encode `((text . ,str)))))
    (potato--url-retrieve (format "/channel/%s/create" potato--channel-id) "POST"
                          (lambda (data) nil))))

(defun potato--parse-json-decode-span (text face)
  (let ((s (potato--parse-json-decode-element text)))
    (propertize s 'font-lock-face face)))

(defun potato--parse-json-decode-url (element)
  (let ((addr (potato--assoc-with-check 'addr element))
        (description (potato--assoc-with-check 'description element)))
    (propertize description
                'mouse-face 'hilight
                'help-echo (format "mouse-2: browse url: %s" addr))))

(defun potato--parse-json-decode-element (element)
  (etypecase element
    (string element)
    (array (apply #'concat (loop for e across element collect (potato--parse-json-decode-element e))))
    (cons (let ((type (potato--assoc-with-check 'type element)))
            (cond ((string= type "p")
                   (potato--parse-json-decode-element (potato--assoc-with-check 'e element)))
                  ((string= type "b")
                   (potato--parse-json-decode-span (potato--assoc-with-check 'e element) 'bold))
                  ((string= type "i")
                   (potato--parse-json-decode-span (potato--assoc-with-check 'e element) 'italic))
                  ((string= type "code")
                   (format "[%s]" (potato--parse-json-decode-span (potato--assoc-with-check 'e element) 'default)))
                  ((string= type "url")
                   (potato--parse-json-decode-url element))
                  (t
                   (format "[unknown-element %s]" type)))))))

(defun potato--parse-json-message (content)
  (potato--parse-json-decode-element content))

(defun potato--process-channel-message (message)
  (let* ((message-id (potato--assoc-with-check 'id message))
         (timestamp (potato--assoc-with-check 'created_date message))
         (text (potato--assoc-with-check 'text message))
         (from (potato--assoc-with-check 'from message))
         (parsed (potato--parse-json-message text))
         (user (cl-assoc from potato--users :test #'equal)))
    (potato--insert-message message-id timestamp (if user (second user) "Unknown") parsed)))

(defun potato--process-channel-type-notification (message)
  (message "typing: %S" message))

(defun potato--process-new-message (message)
  (let ((type (potato--assoc-with-check 'type message)))
    (cond ((equal type "m")
           (potato--process-channel-message (potato--assoc-with-check 'c message)))
          ((string= type "type")
           (potato--process-channel-type-notification message)))))

(defun potato--fetch-message (queue buffer)
  (when potato--connection
    (error "Attempt to fetch a new message while a current request is already active"))
  (with-current-buffer buffer
    (let ((connection (potato--url-retrieve (format "/channel-updates?channels=%s&format=json&services=content,state%s"
                                                    potato--channel-id
                                                    (if queue (format "&event-id=%s" queue) ""))
                                            "GET"
                                            (lambda (data)
                                              (setq potato--connection nil)
                                              (loop for message across (cdr (assoc 'data data))
                                                    do (potato--process-new-message message))
                                              (let ((queue (cdr (assoc 'event data))))
                                                (unless queue
                                                  (error "No queue in channel update"))
                                                (potato--fetch-message queue buffer))))))
      (setq potato--connection connection))))

(cl-defun potato--load-history (&key (num-messages 50))
  (potato--url-retrieve (format "/channel/%s/history?format=json&num=%d" potato--channel-id num-messages)
                        "GET"
                        (lambda (data)
                          (loop for message across (potato--assoc-with-check 'messages data)
                                do (potato--process-channel-message message)))))

(defun potato--buffer-closed ()
  (let ((connection potato--connection))
    (when connection
      (let ((proc (get-buffer-process connection)))
        (when proc
          (message "Stopping outstanding connection")
          (kill-process proc))))))

(defun potato--request-user-list (callback)
  (potato--url-retrieve (format "/channel/%s/users" potato--channel-id)
                        "GET"
                        (lambda (data)
                          (funcall callback
                                   (loop for ch across (potato--assoc-with-check 'members data)
                                         collect (cons (potato--assoc-with-check 'id ch)
                                                       (list (potato--assoc-with-check 'description ch)
                                                             (potato--assoc-with-check 'image_name ch))))))))

(defun potato--create-buffer (name cid)
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (potato-channel-mode)
      (setq-local potato--channel-id cid)
      (setq-local potato--active-url potato-url)
      (setq-local potato--active-api-token potato-api-token)
      (setq-local potato--users nil)
      (setq-local potato--connection nil)
      (setq potato--input-function 'potato--input)
      (make-local-variable 'potato--connection)
      (potato--request-user-list (lambda (users)
                                   (setq potato--users users)
                                   (potato--load-history)))
      (potato--fetch-message nil buffer)
      (add-hook 'kill-buffer-hook 'potato--buffer-closed nil t))
    buffer))

(defun potato--find-channel-buffer (cid)
  (let* ((name (format "*potato-%s*" cid))
         (buffer (or (get-buffer name)
                     (potato--create-buffer name cid))))
    buffer))

(defun potato-client ()
  (interactive)
  (let ((buffer (potato--find-channel-buffer potato-channel-id)))
    (switch-to-buffer buffer)))

(provide 'potato)
