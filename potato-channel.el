;;; -*- lexical-binding: t -*-

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
      (when potato--input-function
        (funcall potato--input-function text)))))

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
  (setq-local potato--input-function nil)
  (insert "channel> ")
  (add-text-properties (point-min) (point-max) '(read-only t rear-nonsticky t front-sticky (read-only)))
  (set-marker-insertion-type potato--output-marker t)
  (set-marker potato--input-marker (point-max)))

(cl-defun debug-insert-message (&optional (time 0))
  (let ((timestamp (format "20150101T12:00:%02dZ" time))
        (message-id (format "msgid%d" time)))
    (potato--insert-message message-id timestamp "Palle" "Hello test message here")))

(defun potato--after-user-change-modification (start end &rest ignore)
  (let ((inhibit-modification-hooks t))
    (let ((real-start (max 1 (1- start)))
          (real-end   (min (1+ (buffer-size)) (1+ end)))
          (any-change nil))
      (save-excursion
        nil))))

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
            (overlay-put overlay 'potato-user-ref (first element))))))))

(provide 'potato-channel)
