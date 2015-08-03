;;; -*- lexical-binding: t -*-

(defun potato-send-input-line ()
  (interactive)
  (let ((text (buffer-substring potato--input-marker (point-max))))
    (delete-region potato--input-marker (point-max))
    (message "Send text: %S" text)))

(defvar potato-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'potato-send-input-line)
    map))

(defun potato--insert-message (message-id timestamp from text)
  (save-excursion
    (goto-char potato--output-marker)
    (let ((inhibit-read-only t))
      (insert (propertize (format "[%s - %s] %s\n" timestamp from text)
                          'read-only t
                          'potato-message-id message-id
                          'potato-timestamp timestamp)))))

(define-derived-mode potato-channel-mode nil "Potato"
  "Mode for Potato channel content"
  (use-local-map potato-channel-mode-map)
  (setq-local potato--output-marker (make-marker))
  (setq-local potato--input-marker (make-marker))
  (set-marker potato--output-marker (point-max))
  (insert "channel> ")
  (add-text-properties (point-min) (point-max) '(read-only t rear-nonsticky t front-sticky (read-only)))
  (set-marker-insertion-type potato--output-marker t)
  (set-marker potato--input-marker (point-max)))

(cl-defun debug-insert-message (&optional (time 0))
  (let ((timestamp (format "20150101T12:00:%02dZ" time))
        (message-id (format "msgid%d" time)))
    (potato--insert-message message-id timestamp "Palle" "Hello test message here")))

(provide 'potato-channel)
