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

(define-derived-mode potato-channel-mode nil "Potato"
  "Mode for Potato channel content"
  (use-local-map potato-channel-mode-map)
  (setq-local potato--output-marker (make-marker))
  (setq-local potato--input-marker (make-marker))
  (set-marker potato--output-marker (point-max))
  (insert "channel> ")
  (set-marker potato--input-marker (point-max)))

(provide 'potato-channel)
