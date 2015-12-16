;;; -*- lexical-binding: t -*-

(defun potato-send-input-line ()
  (interactive)
  (let ((text (string-trim (buffer-substring potato--input-marker (point-max)))))
    (when (not (equal text ""))
      (delete-region potato--input-marker (point-max))
      (when potato--input-function
        (funcall potato--input-function text)))))

(defvar potato-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'potato-send-input-line)
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
                                    (propertize text 'face 'potato-default)
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

(provide 'potato-channel)
