;;; -*- lexical-binding: t -*-

(require 'lui)
(require 'url)

(defgroup potato nil
  "Potato client implementation for Emacs"
  :prefix 'potato
  :group 'applications)

(defcustom potato-api-token ""
  "API token for the user"
  :type 'string
  :group 'potato)

(defcustom potato-url "http://127.0.0.1/"
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
                          (potato--url-handler status buffer callback))))))

(defun potato--input (str)
  (let ((url-request-data (json-encode `((text . ,str)))))
    (potato--url-retrieve (format "/channel/%s/create" potato--channel-id) "POST"
                          (lambda (data) (message "Message sent: %S" data)))))

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
                   (princ "[unknown-element]")))))))

(defun potato--parse-json-message (content)
  (potato--parse-json-decode-element content))

(defun potato--process-channel-message (message)
  (let* ((text (potato--assoc-with-check 'text message))
         (from-name (potato--assoc-with-check 'from_name message))
         (parsed (potato--parse-json-message text)))
    (message "Text: %S" parsed)
    (lui-insert (format "%s: %s" from-name parsed))))

(defun potato--process-new-message (message)
  (let ((type (potato--assoc-with-check 'type message)))
    (when (potato--string-match-start type "msg-")
      (potato--process-channel-message (potato--assoc-with-check 'element message)))))

(defun potato--fetch-message (queue buffer)
  (with-current-buffer buffer
    (let ((connection (potato--url-retrieve (format "/channel/%s/updates?format=json%s"
                                                    potato--channel-id
                                                    (if queue (format "&event-id=%s" queue) ""))
                                            "GET"
                                            (lambda (data)
                                              (loop for message across (cdr (assoc 'data data))
                                                    do (potato--process-new-message message))
                                              (let ((queue (cdr (assoc 'event data))))
                                                (unless queue
                                                  (error "No queue in channel update"))
                                                (potato--fetch-message queue buffer))))))
      (setq potato--connection connection))))

(defun potato--buffer-closed ()
  (let ((connection potato--connection))
    (when connection
      (message "Need to stop outstanding connection"))))

(define-derived-mode potato-mode lui-mode "Potato"
  "Potato channel mode"
  (lui-set-prompt "channel> ")
  (goto-char (point-max))
  (setq lui-input-function 'potato--input))

(defun potato--create-buffer (name cid)
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (potato-mode)
      (setq-local potato--channel-id cid)
      (setq-local potato--active-url potato-url)
      (setq-local potato--active-api-token potato-api-token)
      (make-local-variable 'potato--connection)
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
