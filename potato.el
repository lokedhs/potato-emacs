;;; -*- lexical-binding: t -*-

(require 'url)
(require 'subr-x)
(require 'shr)
(require 'notifications)

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

(defcustom potato-url "https://potato.dhsdevelopments.com/"
  "The main url for the Potato instance"
  :type 'string
  :group 'potato)

(defvar potato-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") 'potato-insert-nl)
    (define-key map (kbd "RET") 'potato-send-input-line)
    (define-key map (kbd "@") 'potato-insert-user)
    map))

(defvar potato--active-buffers nil)
(defvar potato--connection nil
  "The current HTTP connection that is waiting for data, or nil if not currently connected to the server.")
(defvar potato--event-id nil
  "The event id for the current connection, or nil if client not started")
(defvar potato-display-notifications-string "")
(defvar potato--notifications nil)
(defvar potato--unread-channels nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun potato--string-match-start (string key)
  (and (>= (length string) (length key))
       (string= (subseq string 0 (length key)) key)))

(defun potato--assoc-with-check (tag alist &optional allow-missing)
  (let ((value (assoc tag alist)))
    (cond (value
           (cdr value))
          (allow-missing
           nil)
          (t
           (error "No value for tag: %s" tag)))))

(defun potato--make-random-string (n)
  (with-output-to-string
    (loop repeat n
          do (princ (format "%c" (+ ?a (random (1+ (- ?z ?a)))))))))

(defun potato--make-temp-directory (prefix)
  (cl-labels ((try-make-dir (name)
                 (condition-case condition
                     (progn
                       (make-directory name)
                       t)
                   (error nil))))
    (loop repeat 25
          for dirname = (format "%s%s-%s" temporary-file-directory prefix (potato--make-random-string 40))
          when (try-make-dir dirname)
          return dirname
          finally (error "Unable to create temporary directory"))))

(defun potato--format-date (date)
  (format-time-string "%a %d %b %Y, %H:%M:%S" (date-to-time date)))

(cl-defmacro potato--with-channel (cid &body body)
  (declare (indent 1))
  (let ((buffer-sym (gensym "buffer")))
    `(let ((,buffer-sym (potato--find-channel-buffer ,cid)))
       (when ,buffer-sym
         (with-current-buffer ,buffer-sym
           ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Network tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun potato--json-parse-result-buffer ()
  (let* ((content (buffer-substring (point) (point-max)))
         (decoded-content (decode-coding-string content 'utf-8)))
    (json-read-from-string decoded-content)))

(defun potato--url-handler (status buffer callback as-json-p)
  (let ((error-status (getf status :error)))
    (if error-status
        (progn
          (message "Got error: %S" status)
          (signal (car error-status) (cdr error-status)))
      ;; ELSE: No error
      (progn
        (goto-char (point-min))
        (search-forward "\n\n")
        (let ((data (if as-json-p
                        (potato--json-parse-result-buffer)
                      (buffer-substring (point) (point-max)))))
          (with-current-buffer buffer
            (funcall callback data))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer (current-buffer))))))))

(cl-defmacro potato--with-url-params ((url-sym url method) &body body)
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

(cl-defun potato--url-retrieve (url method callback &key (as-json-p t) check-if-shutdown)
  (let ((buffer (current-buffer)))
    (potato--with-url-params (result-url url method)
      (url-retrieve result-url (lambda (status)
                                 (unless (and check-if-shutdown potato--shutdown-in-progress)
                                   (potato--url-handler status buffer callback as-json-p)))
                    nil t))))

(cl-defun potato--url-retrieve-synchronous (url method)
  (potato--with-url-params (result-url url method)
    (let ((buffer (url-retrieve-synchronously result-url)))
      (let ((data (with-current-buffer buffer
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (potato--json-parse-result-buffer))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))
        data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun potato--input (str)
  (let ((url-request-data (encode-coding-string (json-encode `((text . ,str))) 'utf-8)))
    (potato--url-retrieve (format "/channel/%s/create" potato--channel-id) "POST"
                          (lambda (data) nil))))

(defun potato-send-input-line ()
  "Send the currently typed line to the server."
  (interactive)
  (let ((text (string-trim (potato--read-input-line potato--input-marker (point-max)))))
    (when (not (equal text ""))
      (delete-region potato--input-marker (point-max))
      (potato--input text))))

(defun potato-insert-nl ()
  "Insert a newline into the message."
  (interactive)
  (insert "\n"))

(defun potato-switch-to-next-unread ()
  "Switch to the first channel with unread messages."
  (interactive)
  (let ((buffer (loop for channel in (potato--get-all-unread-channels)
                      for buffer = (potato--find-channel-buffer (first channel) :create-if-missing t)
                      when (not (eq buffer (current-buffer)))
                      return buffer)))
    (if buffer
        (switch-to-buffer buffer)
      (message "No channels with unread messages"))))

(defun potato--insert-message (message-id timestamp updated-date from text image extra-html)
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
        (let ((start (point)))
          (insert (concat (propertize (concat (format "[%s] %s" from (potato--format-date timestamp))
                                              (if updated-date
                                                  (format " (updated %s)" (potato--format-date updated-date))
                                                "")
                                              "\n")
                                      'face 'potato-message-from)
                          text
                          "\n\n"))
          (when image
            (potato--insert-image (potato--assoc-with-check 'file image))
            (insert "\n"))
          (when extra-html
            (let ((extra-html-start (point)))
              (insert extra-html)
              (shr-render-region extra-html-start (point))))
          (add-text-properties start (point)
                               (list 'read-only t
                                     'potato-message-id message-id
                                     'potato-timestamp timestamp
                                     'front-sticky '(read-only))))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Image support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun potato--insert-image-handler (overlay data)
  (let ((image (create-image data nil t))
        (start (overlay-start overlay))
        (end (overlay-end overlay)))
    (delete-overlay overlay)
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char start)
        (delete-region start end)
        (let ((start (point)))
          (insert-image image "[image]")
          (potato--extend-message-text-properties start (point)))))))

(cl-defun potato--insert-image (file)
  (let ((buffer (current-buffer))
        (start (point)))
    (insert "[loading-image]")
    (let ((overlay (make-overlay start (point))))
      (potato--url-retrieve file "GET"
                            (lambda (data)
                              (potato--insert-image-handler overlay data))
                            :as-json-p nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maths rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To render an expression:
;;;   latex -halt-on-error -output-directory=z foo.tex
;;;   dvipng -o foo.png -bg transparent -q -T tight -z 9 z/foo.dvi

(defun potato--render-maths-to-image (latex-expression)
  (let* ((dirname (potato--make-temp-directory "potato-render"))
         (file-prefix (format "%s/math-formula" dirname)))
    (with-temp-buffer
      (insert "\\documentclass[12pt]{article}")
      (insert "\\pagestyle{empty}")
      (insert "\\begin{document}")
      (insert "\\(")
      (insert latex-expression)
      (insert "\\)")
      (insert "\\end{document}")
      (write-file (format "%s.tex" file-prefix)))
    (if (not (zerop (shell-command (format "latex -halt-on-error -output-directory=%s %s.tex" dirname file-prefix))))
        (message "Illegal formula, can't render with LaTeX")
      (if (not (zerop (shell-command "dvipng -o %s.png -bg transparent -q -T tight -z 9 %s.dvi" file-prefix file-prefix)))
          (message "Unable to convert formula to png")
        (format "%s.png" file-prefix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun potato--parse-json-decode-span (text face)
  (let ((s (potato--parse-json-decode-element text)))
    (propertize s 'font-lock-face face)))

(defvar potato-url-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'potato-open-selected-link)
    (define-key map (kbd "RET") 'potato-open-selected-link)
    map))

(defun potato-open-selected-link ()
  "Open the link at the current position."
  (interactive)
  (when-let ((url (get-char-property (point) 'potato-link-destination)))
    (browse-url url)))

(defun potato--parse-json-decode-url (element)
  (let ((addr (potato--assoc-with-check 'addr element))
        (description (potato--assoc-with-check 'description element)))
    (propertize description
                'font-lock-face 'link
                'mouse-face 'highlight
                'help-echo (format "mouse-2: browse url: %s" addr)
                'potato-link-destination addr
                'keymap potato-url-keymap)))

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
                  ((string= type "math")
                   (format "\n  %s\n" (potato--parse-json-decode-element (potato--assoc-with-check 'e element))))
                  ((string= type "inline-math")
                   (format "[%s]" (potato--parse-json-decode-element (potato--assoc-with-check 'e element))))
                  (t
                   (format "[unknown-element %s]" type)))))))

(defun potato--parse-json-message (content)
  (potato--parse-json-decode-element content))

(defun potato--extend-message-text-properties (start end)
  (let* ((pos (1- start))
         (message-id (get-text-property pos 'potato-message-id))
         (timestamp (get-text-property pos 'potato-timestamp)))
    (unless (and message-id timestamp)
      (error "No message text properties at position %d" pos))
    (add-text-properties start end (list 'read-only t
                                         'potato-message-id message-id
                                         'potato-timestamp timestamp
                                         'front-sticky t))))

(defun potato--find-message-in-log (message-id)
  (loop with curr = (point-min)
        for pos = (next-single-property-change curr 'potato-message-id)
        while pos
        for value = (get-char-property pos 'potato-message-id)
        when (equal value message-id)
        return (list pos (next-single-property-change pos 'potato-message-id))
        do (setq curr pos)
        finally (return nil)))

(defun potato--process-channel-message (message loading-history)
  (with-current-buffer (potato--find-channel-buffer (potato--assoc-with-check 'channel message))
    (let ((message-id (potato--assoc-with-check 'id message))
          (updated (potato--assoc-with-check 'updated message t)))
      (when (or loading-history
                (not updated)
                (let ((old-message-pos (potato--find-message-in-log message-id)))
                  (if old-message-pos
                      (destructuring-bind (start end) old-message-pos
                        (let ((inhibit-read-only t))
                          (delete-region start end))
                        t)
                    nil)))
        (when (not (potato--assoc-with-check 'deleted message t))
          (let* ((timestamp (potato--assoc-with-check 'created_date message))
                 (text (potato--assoc-with-check 'text message))
                 (from (potato--assoc-with-check 'from message))
                 (parsed (potato--parse-json-message text))
                 (user (cl-assoc from potato--users :test #'equal))
                 (image (let ((image-entry (assoc 'image message)))
                          (if image-entry (cdr image-entry) nil)))
                 (extra-html (potato--assoc-with-check 'extra_html message t))
                 (updated-date (potato--assoc-with-check 'updated_date message t)))
            (potato--insert-message message-id timestamp updated-date
                                    (if user (second user) "Unknown")
                                    (string-trim parsed)
                                    image
                                    extra-html)
            (when (and (not loading-history)
                       (not (get-buffer-window (current-buffer) 'visible)))
              (let ((old potato--unread-in-channel))
                (incf potato--unread-in-channel)
                (when (zerop old)
                  (potato--recompute-modeline))))))))))

(defun potato--process-channel-type-notification (message)
  (let* ((user (potato--assoc-with-check 'user message))
         (cid (potato--assoc-with-check 'channel message))
         (add-type (potato--assoc-with-check 'add-type message))
         (buffer (potato--find-channel-buffer cid :create-if-missing nil)))
    (when buffer
      (with-current-buffer buffer
        (cond ((equal add-type "begin")
               (cl-pushnew user potato--current-typing :test #'equal))
              ((equal add-type "end")
               (setq potato--current-typing (cl-remove user potato--current-typing :test #'equal)))
              (t
               (error "Unexpected typing mode: ~S" add-type)))
        (potato--recompute-channel-modeline)))))

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

(defun potato--update-active-state-from-sync (message)
  (let* ((uids (mapcar (lambda (v)
                         (potato--assoc-with-check 'id v))
                       (potato--assoc-with-check 'users message)))
         (unregistered-users (cl-remove-if (lambda (v)
                                             (cl-member v potato--users :key #'car :test #'equal))
                                           uids)))
    (loop for row in potato--users
          do (setf (fourth row) (if (cl-member (first row) uids :test #'equal) t nil)))
    (dolist (uid unregistered-users)
      (potato--update-active-state-for-user-from-id uid t))))

(defun potato--process-channel-update-user (message)
  (potato--with-channel (potato--assoc-with-check 'channel message t)
    (let ((type (potato--assoc-with-check 'add-type message)))
      (cond ((equal type "add")
             (potato--update-active-state-for-user-from-message message t))
            ((equal type "remove")
             (potato--update-active-state-for-user-from-message message nil))
            ((equal type "sync")
             (potato--update-active-state-from-sync message))
            (t
             (message "Unexpected user update message: %S" message))))))

(defun potato--process-notification (message)
  (let* ((cid (potato--assoc-with-check 'channel message))
         (e (cl-find cid potato--notifications :key #'car :test #'equal)))
    (if e
        (incf (cdr e))
      (push (cons cid 1) potato--notifications)))
  (potato--recompute-modeline))

(defun potato--request-channel-info (cid callback)
  (potato--url-retrieve (format "/channel/%s" cid) "GET"
                        (lambda (data)
                          (funcall callback data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unread processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun potato--find-unread-channel-data (cid)
  (cl-find cid potato--unread-channels :key #'car :test #'equal))

(defun potato--process-unread (message)
  (let* ((cid (potato--assoc-with-check 'channel message))         
         (e (potato--find-unread-channel-data cid))
         (n (potato--assoc-with-check 'count message)))
    (if e
        (progn
          (setf (third e) n)
          (potato--recompute-modeline))
      (progn
        (push (list cid "[unknown]" n) potato--unread-channels)
        (potato--request-channel-info cid
                                      (lambda (data)
                                        (let ((info (potato--find-unread-channel-data cid)))
                                          (when info
                                            (let ((channel-name (potato--assoc-with-check 'name data)))
                                              (setf (second info) channel-name)
                                              (potato--recompute-modeline))))))))))

(defun potato--process-channel-change (message)
  (let ((cid (potato--assoc-with-check 'channel message))
        (name (potato--assoc-with-check 'name message))
        (topic (potato--assoc-with-check 'topic message)))
    (potato--with-channel cid
      (unless (equal name potato--name)
        (potato--update-channel-name-in-buffer name))
      (setq potato--topic topic))))

(defun potato--get-all-unread-channels ()
  (let ((opened-channels (mapcan (lambda (v)
                                   (destructuring-bind (cid . buffer) v
                                     (with-current-buffer buffer
                                       (when (and potato--name (plusp potato--unread-in-channel))
                                         (list (list cid potato--name))))))
                                 potato--active-buffers)))
    (let ((remote (loop for (cid name unread-count) in potato--unread-channels
                        when (and (plusp unread-count)
                                  (not (cl-member cid opened-channels :key #'first :test #'equal)))
                        collect (list cid name))))
      (append opened-channels remote))))

(defun potato--make-unread-notification-string ()
  (if-let ((result (potato--get-all-unread-channels)))
      (with-output-to-string
        (princ "Unread: ")
        (loop for channel in result
              for first = t then nil
              unless first
              do (princ "/")
              do (princ (second channel))))))

(defun potato--add-remove-binding (cid add-p)
  (unless potato--event-id
    (error "Client has not been started"))
  (let ((url (with-output-to-string
               (princ "/channel-updates/update?event-id=")
               (princ potato--event-id)
               (princ "&cmd=")
               (princ (if add-p "add" "remove"))
               (princ "&channel=")
               (princ cid)
               (princ "&services=content,state,notifications,unread"))))
    (potato--url-retrieve url "POST" (lambda (data)
                                       (let ((result (assoc 'result data)))
                                         (unless (and result (equal (cdr result) "ok"))
                                           (message "Unable to connect to channel")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main event processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun potato--process-new-message (message)
  (let ((type (potato--assoc-with-check 'type message)))
    (cond ((equal type "m")
           (potato--process-channel-message (potato--assoc-with-check 'c message) nil))
          ((equal type "type")
           (potato--process-channel-type-notification message))
          ((equal type "cu")
           (potato--process-channel-update-user message))
          ((equal type "usernot")
           (potato--process-notification message))
          ((equal type "unread")
           (potato--process-unread message))
          ((equal type "channel-change")
           (potato--process-channel-change message))
          (t
           (message "Unprocessed message: %S" message)))))

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
                (princ "&format=json&services=content,state,channel,notifications,unread")
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
                                                  (message "Unexpected result from update: %S" data)
                                                  (error "No queue in channel update"))
                                                (potato--fetch-message queue)))
                                            :check-if-shutdown t)))
      (with-current-buffer connection
        (setq-local potato--shutdown-in-progress nil))
      (setq potato--connection connection))))

(cl-defun potato--load-history (&key (num-messages 50))
  (potato--url-retrieve (format "/channel/%s/history?format=json&num=%d" potato--channel-id num-messages)
                        "GET"
                        (lambda (data)
                          (loop for message across (potato--assoc-with-check 'messages data)
                                do (potato--process-channel-message message t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun potato--enable-buffer (buffer)
  (with-current-buffer buffer
    (let ((active-buffers potato--active-buffers))
      (push (cons potato--channel-id buffer) potato--active-buffers)
      (cond ((null active-buffers)
             (potato--fetch-message nil))
            (t
             (potato--add-remove-binding potato--channel-id t))))))

(defun potato--buffer-closed ()
  (setq potato--active-buffers (cl-remove (current-buffer) potato--active-buffers :key #'cdr :test #'eq))
  (when (and (null potato--active-buffers)
             (member 'potato-display-notifications-string global-mode-string))
    (setq global-mode-string (remove 'potato-display-notifications-string global-mode-string)))
  (let ((connection potato--connection))
    (when connection
      (if potato--active-buffers
          ;; We still have other opened buffers, simply unregister the binding
          (when nil
            ;; TODO: Remove isn't supported by the server yet, so let's just keep the binding active
            (potato--add-remove-binding potato--channel-id nil))
        ;; ELSE: This was the last buffer, close the connection
        (let ((proc (get-buffer-process connection)))
          (when proc
            (message "All channel windows closed. Disconnecting from server.")
            (with-current-buffer connection
              (setq-local potato--shutdown-in-progress t))
            (setq potato--event-id nil)
            (setq potato--unread-channels nil)
            (condition-case condition
                (delete-process proc)
              (error (message "Error when closing buffer: %S" condition)))))))))

(defun potato--update-channel-name-in-buffer (name)
  "Updates the local buffer configration as well as its name to reflect the actual name of the channel."
  (setq potato--name name)
  (rename-buffer (format "Potato - %s" name) t))

(defun potato--window-config-updated ()
  "Hook function that is locally installed for window-configuration-change-hook in all chanbel buffers."
  (when (get-buffer-window)
    (let ((e (cl-find potato--channel-id potato--notifications :key #'car :test #'equal)))
      (when (and e (plusp (cdr e)))
        (potato--url-retrieve (format "/channel/%s/clear-notifications" potato--channel-id)
                              "POST"
                              (lambda (data)
                                nil))
        (setf (cdr e) 0)
        (potato--recompute-modeline)))))

(defun potato--create-buffer (name cid)
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (potato-channel-mode)
      (setq-local potato--channel-id cid)
      (setq-local potato--users nil)
      (setq-local potato--pending-user-state nil)
      (setq-local potato--last-typing-notifcation nil)
      (setq-local potato--current-typing nil)
      (setq-local potato--channel-mode-line "")
      (setq-local potato--unread-in-channel 0)
      (setq-local potato--name nil)
      (setq-local potato--topic nil)
      (setq mode-line-format (append mode-line-format (list 'potato--channel-mode-line)))
      (potato--request-channel-info cid
                                    (lambda (data)
                                      (potato--update-channel-name-in-buffer (potato--assoc-with-check 'name data))
                                      (potato--request-user-list (lambda (users)
                                                                   (potato--update-userlist users)
                                                                   (potato--load-history)))))
      (potato--enable-buffer buffer)
      (add-hook 'kill-buffer-hook 'potato--buffer-closed nil t)
      (add-hook 'post-self-insert-hook 'potato--send-typing-notification t t)
      (add-hook 'window-configuration-change-hook 'potato--window-config-updated nil t))
    ;; Update the modeline indicator if needed
    (unless (member 'potato-display-notifications-string global-mode-string)
      (if global-mode-string
          (setq global-mode-string (append global-mode-string '(potato-display-notifications-string)))
        (setq global-mode-string '("" potato-display-notifications-string)))
      (potato--recompute-modeline))
    buffer))

(cl-defun potato--find-channel-buffer (cid &key create-if-missing)
  (let ((e (find cid potato--active-buffers :key #'car :test #'equal)))
    (cond (e
           (cdr e))
          (create-if-missing
           (potato--create-buffer (format "*potato-%s*" cid) cid))
          (t
           (error "No buffer for channel %s" cid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun potato--request-user-list (callback)
  (potato--url-retrieve (format "/channel/%s/users" potato--channel-id)
                        "GET"
                        (lambda (data)
                          (funcall callback
                                   (loop for ch across (potato--assoc-with-check 'members data)
                                         collect (cons (potato--assoc-with-check 'id ch)
                                                       (list (potato--assoc-with-check 'description ch)
                                                             (potato--assoc-with-check 'nickname ch)
                                                             (potato--assoc-with-check 'image_name ch))))))))

(defun potato--update-userlist (users)
  (setq potato--users users)
  (dolist (v potato--pending-user-state)
    (destructuring-bind (uid new-state) v
      (potato--update-active-state-for-user-from-id uid new-state)))
  (setq potato--pending-user-state nil))

(defun potato--name-for-uid (uid)
  (let ((v (potato--assoc-with-check uid potato--users t)))
    (if v
        (car v)
      uid)))

(defun potato--send-typing-notification ()
  (let ((now (float-time)))
    (when (or (null potato--last-typing-notifcation)
              (> now (+ potato--last-typing-notifcation 2)))
      (let ((url-request-data (encode-coding-string (json-encode '((state . t))) 'utf-8)))
        (potato--url-retrieve (format "/channel/%s/type" potato--channel-id) "POST"
                              (lambda (data)
                                (unless (equal (potato--assoc-with-check 'result data) "ok")
                                  (message "Error when sending typing notification")))))
      (setq potato--last-typing-notifcation now))))

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

(defun potato--make-separated-string (&rest args)
  (with-output-to-string
    (loop with first = t
          for v in args
          when v
          do (progn
               (unless first
                 (princ " "))
               (princ v)
               (setq first nil)))))

(defun potato--recompute-modeline ()
  (setq potato-display-notifications-string
        (potato--make-separated-string
         (if potato--notifications
             (let ((n (reduce #'+ (mapcar #'cdr potato--notifications))))
               (if (plusp n)
                   (propertize (format "Potato:%d" n)
                               'face 'potato-notification))))
         (potato--make-unread-notification-string)))
  (force-mode-line-update t))

(defun potato--recompute-channel-modeline ()
  (setq potato--channel-mode-line
        (potato--make-separated-string
         (if potato--current-typing
             (with-output-to-string
               (if (null (cdr potato--current-typing))
                   (princ (format "%s is typing" (potato--name-for-uid (car potato--current-typing))))
                 (loop for (uid . rest) on potato--current-typing
                       if rest
                       do (princ (format "%s, " (potato--name-for-uid uid)))
                       else
                       do (princ (format "%s are typing") (potato--name-for-uid uid))))))))
  (force-mode-line-update))

(defun potato-client (channel-id)
  (interactive (list (potato--choose-channel-id)))
  (unless (and potato-api-token (plusp (length potato-api-token)))
    (user-error "Set the variable ‘potato-api-token’ before starting the Potato client."))
  (let ((buffer (potato--find-channel-buffer channel-id :create-if-missing t)))
    (switch-to-buffer buffer)))

(provide 'potato)
