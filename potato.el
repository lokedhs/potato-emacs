;;; -*- lexical-binding: t -*-

(require 'url)

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

(defface potato-message-user-name
  '((((class color))
     :background "#e0e0e0"
     :inherit potato-default)
    (t
     :inherit potato-default))
  "Face used to display user names."
  :group 'potato)

(defface potato-message-input-user-name
  '((((class color))
     :background "#e0e0e0"
     :inherit potato-default)
    (t
     :inherit potato-default))
  "Face used to display user names."
  :group 'potato)

(defface potato-message-code
  '((((class color))
     :background "#f0f0f0"
     :inherit potato-default)
    (t
     :inherit potato-default))
  "Face used to display code snippets."
  :group 'potato)

(defface potato-notification
  '((((class color))
     :foreground "#ff0000"
     :inherit potato-default)
    (t
     :inherit potato-default))
  "Face used to display the unread notifications number in the modeline."
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

(defvar potato--active-buffers nil)
(defvar potato--connection nil
  "The current HTTP connection that is waiting for data, or nil if not currently connected to the server.")
(defvar potato--event-id nil
  "The event id for the current connection, or nil if client not started")

(defun potato--string-match-start (string key)
  (and (>= (length string) (length key))
       (string= (subseq string 0 (length key)) key)))

(defun potato--assoc-with-check (tag alist)
  (let ((value (assoc tag alist)))
    (unless value
      (error "No value for tag: %s" tag))
    (cdr value)))

(defun potato--json-parse-result-buffer ()
  (goto-char (point-min))
  (search-forward "\n\n")
  (let* ((content (buffer-substring (point) (point-max)))
         (decoded-content (decode-coding-string content 'utf-8)))
    (json-read-from-string decoded-content)))

(defun potato--url-handler (status buffer callback)
  (let ((error-status (getf status :error)))
    (if error-status
        (progn
          (message "Got error: %S" status)
          (signal (car error-status) (cdr error-status)))
      ;; ELSE: No error
      (let ((data (potato--json-parse-result-buffer)))
        (with-current-buffer buffer
          (funcall callback data))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer (current-buffer)))))))

(cl-defmacro with-url-params ((url-sym url method) &body body)
  (declare (indent 1))
  (let ((base-sym (gensym "base-")))
    `(let* ((,base-sym potato-url))
       (let ((url-request-method ,method)
             (url-request-extra-headers (list (cons "API-Token" potato-api-token)))
             (,url-sym (format "%s%sapi/1.0%s"
                               ,base-sym
                               (if (eql (aref ,base-sym (1- (length ,base-sym))) ?/) "" "/")
                               ,url)))
         ,@body))))

(defun potato--url-retrieve (url method callback)
  (let ((buffer (current-buffer)))
    (with-url-params (result-url url method)
      (url-retrieve result-url (lambda (status)
                                 (potato--url-handler status buffer callback))
                    nil t))))

(cl-defun potato--url-retrieve-synchronous (url method)
  (with-url-params (result-url url method)
    (let ((buffer (url-retrieve-synchronously result-url)))
      (let ((data (with-current-buffer buffer
                    (potato--json-parse-result-buffer))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))
        data))))

(defun potato--read-input-line (start end)
  (let ((uid-refs (loop for overlay in (overlays-in start end)
                        for uid = (overlay-get overlay 'potato-user-ref)
                        when uid
                        collect (list (overlay-start overlay) (overlay-end overlay) uid overlay))))
    (with-output-to-string
      (loop with p = start
            for uid-ref in (sort uid-refs (lambda (a b) (< (first a) (first b))))
            if (< p (first uid-ref))
            do (princ (buffer-substring p (first uid-ref)))
            do (progn
                 (princ (format "\U000f0001user:%s:%s\U000f0001"
                                (third uid-ref) (buffer-substring (first uid-ref) (second uid-ref))))
                 (setq p (second uid-ref))
                 (delete-overlay (fourth uid-ref)))
            finally (when (< p end)
                      (princ (buffer-substring p end)))))))

(defun potato-send-input-line ()
  (interactive)
  (let ((text (string-trim (potato--read-input-line potato--input-marker (point-max)))))
    (when (not (equal text ""))
      (delete-region potato--input-marker (point-max))
      (potato--input text))))

(defvar potato-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'potato-send-input-line)
    (define-key map (kbd "@") 'potato-insert-user)
    map))

(defun potato--insert-message (message-id timestamp from text)
  (save-excursion
    (goto-char potato--output-marker)
    (let ((new-pos (loop with prev-pos = (point)
                         for pos = (previous-single-char-property-change prev-pos 'potato-timestamp)
                         until (let ((prop (get-char-property pos 'potato-timestamp)))
                                 (and prop (string< prop timestamp)))
                         do (setq prev-pos pos)
                         until (= pos (point-min))
                         finally
                         return prev-pos)))
      (goto-char new-pos)
      (let ((inhibit-read-only t))
        (insert (propertize (concat (propertize (format "[%s] " from)
                                                'face 'potato-message-from)
                                    text
                                    "\n")
                            'read-only t
                            'potato-message-id message-id
                            'potato-timestamp timestamp
                            'front-sticky '(read-only)))))))

(define-derived-mode potato-channel-mode nil "Potato"
  "Mode for Potato channel content"
  (use-local-map potato-channel-mode-map)
  (setq-local potato--output-marker (make-marker))
  (setq-local potato--input-marker (make-marker))
  (set-marker potato--output-marker (point-max))
  (insert "channel> ")
  (add-text-properties (point-at-bol) (point)
                       (list 'read-only t
                             'rear-nonsticky t
                             'front-sticky '(read-only)
                             'inhibit-line-move-field-capture t
                             'field 'output))
  (set-marker-insertion-type potato--output-marker t)
  (set-marker potato--input-marker (point-max)))

(cl-defun debug-insert-message (&optional (time 0))
  (let ((timestamp (format "20150101T12:00:%02dZ" time))
        (message-id (format "msgid%d" time)))
    (potato--insert-message message-id timestamp "Palle" "Hello test message here")))

(defun potato--after-user-change-modification (start end &rest ignore)
  (let ((inhibit-modification-hooks t))
    (let ((real-start (max 1 (1- start)))
          (real-end (min (1+ (buffer-size)) (1+ end)))
          (any-change nil))
      (save-excursion
        nil))))

(defun potato--user-ref-updated (overlay &rest rest)
  (delete-overlay overlay))

(defun potato-insert-user ()
  "Select a username to be inserted into the new message."
  (interactive)
  (let ((result (let ((completion-ignore-case t))
                  (completing-read "User: " (mapcar #'second potato--users) nil t nil nil nil t))))
    (if (equal result "")
        ;; Blank string, simply insert an @-sign
        (insert "@")
      ;; ELSE: Insert a username tag
      (let ((element (find result potato--users :key #'second :test #'equal)))
        (unless element
          (error "Selected element not found in user list: %S" element))
        (let ((start (point)))
          (insert (second element))
          (let ((overlay (make-overlay start (point) nil nil nil)))
            (overlay-put overlay 'face 'potato-message-input-user-name)
            (overlay-put overlay 'potato-user-ref (first element))
            (overlay-put overlay 'modification-hooks '(potato--user-ref-updated))))))))

(defun potato--input (str)
  (let ((url-request-data (encode-coding-string (json-encode `((text . ,str))) 'utf-8)))
    (potato--url-retrieve (format "/channel/%s/create" potato--channel-id) "POST"
                          (lambda (data) nil))))

(defun potato--parse-json-decode-span (text face)
  (let ((s (potato--parse-json-decode-element text)))
    (propertize s 'font-lock-face face)))

(defun potato--parse-json-decode-url (element)
  (let ((addr (potato--assoc-with-check 'addr element))
        (description (potato--assoc-with-check 'description element)))
    (propertize description
                'font-lock-face 'link
                'mouse-face 'highlight
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
                   (potato--parse-json-decode-span (potato--assoc-with-check 'e element) 'potato-message-code))
                  ((string= type "code-block")
                   (format "\n%s\n" (potato--parse-json-decode-span (potato--assoc-with-check 'code element) 'potato-message-code)))
                  ((string= type "url")
                   (potato--parse-json-decode-url element))
                  ((string= type "user")
                   (propertize (potato--assoc-with-check 'user_description element) 'font-lock-face 'potato-message-user-name))
                  ((string= type "newline")
                   "\n")
                  (t
                   (format "[unknown-element %s]" type)))))))

(defun potato--parse-json-message (content)
  (potato--parse-json-decode-element content))

(defun potato--process-channel-message (message)
  (with-current-buffer (potato--find-channel-buffer (potato--assoc-with-check 'channel message))
    (let* ((message-id (potato--assoc-with-check 'id message))
           (timestamp (potato--assoc-with-check 'created_date message))
           (text (potato--assoc-with-check 'text message))
           (from (potato--assoc-with-check 'from message))
           (parsed (potato--parse-json-message text))
           (user (cl-assoc from potato--users :test #'equal)))
      (potato--insert-message message-id timestamp (if user (second user) "Unknown") (string-trim parsed)))))

(defun potato--process-channel-type-notification (message)
  (message "typing: %S" message))

(defun potato--update-active-state-for-user-from-id (uid new-state)
  (if potato--users
      (let* ((element (find uid potato--users :key #'car :test #'equal)))
        (if element
            (setf (cdr (nthcdr 2 element)) (list new-state))
          ;; ELSE: User was not found in user list, add a stub entry and request an update
          (push (list uid "(loading)" nil new-state) potato--users)))
    ;; ELSE: User list has not been updated yet, add the request to pending
    (cl-pushnew (list uid new-state) potato--pending-user-state :key #'car :test #'equal)))

(defun potato--update-active-state-for-user-from-message (message new-state)
  (potato--update-active-state-for-user-from-id (potato--assoc-with-check 'user message) new-state))

(defun potato--process-channel-update-user (message)
  (let ((type (potato--assoc-with-check 'add-type message)))
    (cond ((string= type "add")
           (potato--update-active-state-for-user-from-message message t))
          ((string= type "remove")
           (potato--update-active-state-for-user-from-message message nil))
          (t
           (message "Unexpected user update message: %S" message)))))

(defvar potato--notifications nil)

(defun potato--process-notification (message)
  (setq xx message)
  (let* ((cid (potato--assoc-with-check 'channel message))
         (e (cl-find cid potato--notifications :key #'car :test #'equal)))
    (if e
        (incf (cdr e))
      (push (cons cid 1) potato--notifications)))
  (potato--recompute-notifications-message))

(defun potato--process-typing (message)
  nil)

(defun potato--process-new-message (message)
  (let ((type (potato--assoc-with-check 'type message)))
    (cond ((equal type "m")
           (potato--process-channel-message (potato--assoc-with-check 'c message)))
          ((string= type "type")
           (potato--process-channel-type-notification message))
          ((string= type "cu")
           (potato--process-channel-update-user message))
          ((string= type "usernot")
           (potato--process-notification message))
          ((string= type "typing")
           (potato--process-typing message))
          (t
           (message "Unprocessed message: %S" message)))))

(defun potato--add-binding (cid)
  (unless potato--event-id
    (error "Client has not been started"))
  (let ((url (with-output-to-string
               (princ "/channel-updates/update?event-id=")
               (princ potato--event-id)
               (princ "&cmd=add&channel=")
               (princ cid)
               (princ "&services=content,state,notifications"))))
    (potato--url-retrieve url "POST" (lambda (data)
                                       (let ((result (assoc 'result data)))
                                         (unless (and result (equal (cdr result) "ok"))
                                           (message "Unable to connect to channel")))))))

(defun potato--fetch-message (queue)
  (when potato--connection
    (error "Attempt to fetch a new message while a current request is already active"))
  (let* ((url (with-output-to-string
                (princ "/channel-updates?channels=")
                (loop for (cid . buffer) in potato--active-buffers
                      for first = t then nil
                      unless first
                      do (princ ",")
                      do (princ cid))
                (princ "&format=json&services=content,state,notifications")
                (when queue
                  (princ "&event-id=")
                  (princ queue)))))
    (let ((connection (potato--url-retrieve url
                                            "GET"
                                            (lambda (data)
                                              (setq potato--connection nil)
                                              (loop for message across (cdr (assoc 'data data))
                                                    do (potato--process-new-message message))
                                              (let ((queue (cdr (assoc 'event data))))
                                                (setq potato--event-id queue)
                                                (unless queue
                                                  (error "No queue in channel update"))
                                                (potato--fetch-message queue))))))
      (setq potato--connection connection))))

(defun potato--enable-buffer (buffer)
  (with-current-buffer buffer
    (let ((active-buffers potato--active-buffers))
      (push (cons potato--channel-id buffer) potato--active-buffers)
      (cond ((null active-buffers)
             (potato--fetch-message nil))
            (t
             (potato--add-binding potato--channel-id))))))

(cl-defun potato--load-history (&key (num-messages 50))
  (potato--url-retrieve (format "/channel/%s/history?format=json&num=%d" potato--channel-id num-messages)
                        "GET"
                        (lambda (data)
                          (loop for message across (potato--assoc-with-check 'messages data)
                                do (potato--process-channel-message message)))))

(defun potato--buffer-closed ()
  (setq potato--active-buffers (cl-remove (current-buffer) potato--active-buffers :key #'cdr :test #'eq))
  (when (and (null potato--active-buffers)
             (member 'potato-display-notifications-string global-mode-string))
    (setq global-mode-string (remove 'potato-display-notifications-string global-mode-string)))
  (let ((connection potato--connection))
    (when connection
      (let ((proc (get-buffer-process connection)))
        (when proc
          (message "Stopping outstanding connection")
          (condition-case condition
              (kill-process proc)
            (error (message "Error when closing buffer: %S" condition))))))))

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
      (setq-local potato--users nil)
      (setq-local potato--pending-user-state nil)
      (make-local-variable 'potato--connection)
      (potato--request-user-list (lambda (users)
                                   (setq potato--users users)
                                   (potato--load-history)))
      (potato--enable-buffer buffer)
      (add-hook 'kill-buffer-hook 'potato--buffer-closed nil t))
    ;; Update the modeline indicator if needed
    (unless (member 'potato-display-notifications-string global-mode-string)
      (if global-mode-string
          (setq global-mode-string (append global-mode-string '(potato-display-notifications-string)))
        (setq global-mode-string '("" potato-display-notifications-string))))
    buffer))

(cl-defun potato--find-channel-buffer (cid &key create-if-missing)
  (let ((e (find cid potato--active-buffers :key #'car :test #'equal)))
    (cond (e
           (cdr e))
          (create-if-missing
           (potato--create-buffer (format "*potato-%s*" cid) cid))
          (t
           (error "No buffer for channel %s" cid)))))

(defun potato--request-channel-list ()
  (let ((result (potato--url-retrieve-synchronous "/channels" "GET")))
    (let ((channels (loop
                     for domain across result
                     for domain-id = (potato--assoc-with-check 'id domain)
                     for domain-name = (potato--assoc-with-check 'name domain)
                     append (loop
                             for group across (potato--assoc-with-check 'groups domain)
                             for group-id = (potato--assoc-with-check 'id group)
                             for group-name = (potato--assoc-with-check 'name group)
                             append (loop
                                     for channel across (potato--assoc-with-check 'channels group)
                                     for channel-id = (potato--assoc-with-check 'id channel)
                                     for channel-name = (potato--assoc-with-check 'name channel)
                                     collect (list channel-id domain-name group-name channel-name))))))
      channels)))

(defun potato--choose-channel-id ()
  (let* ((channels (coerce (potato--request-channel-list) 'vector))
         (names-list (loop for i from 0 below (length channels)
                           for chan = (aref channels i)
                           for channel-name = (fourth chan)
                           unless (or (string-match "^Private channel for users" channel-name))
                           collect (cons (if (loop for i2 from 0 below (length channels)
                                                   when (and (/= i i2) (equal (fourth (aref channels i2)) channel-name))
                                                   return t
                                                   finally (return nil))
                                             (format "%s (%s/%s)" channel-name (second chan) (third chan))
                                           channel-name)
                                         (first chan)))))
    (let ((result (completing-read "Channel: " names-list nil t nil nil nil nil)))
      (let ((e (find result names-list :key #'car :test #'equal)))
        (unless e
          (error "Did not find selected channel"))
        (cdr e)))))

(defvar potato-display-notifications-string "")

(defun potato--recompute-notifications-message ()
  (setq potato-display-notifications-string
        (if potato--notifications
            (propertize (format "Potato:%d" (reduce #'+ (mapcar #'cdr potato--notifications)))
                        'face 'potato-notification)
          ""))
  (force-mode-line-update t))

(defun potato-client (channel-id)
  (interactive (list (potato--choose-channel-id)))
  (let ((buffer (potato--find-channel-buffer channel-id :create-if-missing t)))
    (switch-to-buffer buffer)))

(provide 'potato)
