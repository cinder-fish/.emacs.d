(defvar-local hidden-mode-line-mode nil)

(defvar-local hidden-mode-line nil
  "Stores the mode-line-format when the mode-line is hidden")

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hidden-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hidden-mode-line
          hidden-mode-line nil))
  (force-mode-line-update))

;; If you want to hide the mode-line in every buffer by default
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)
;; (remove-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

(provide 'hidden-mode-line-mode)
