(defun clojure-settings/add-hooks (hooks fns)
  (cl-loop for hook in hooks
           for fn in fns
           do (add-hook hook fn)))

(clojure-settings/add-hooks
 '(clojure-mode-hook
   clojurescript-mode-hook
   cider-mode-hook)
 '(paredit-mode
   rainbow-delimiters-mode))

(setq nrepl-hide-special-buffers t)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-show-error-buffer nil)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(define-key clojure-mode-map (kbd "C-c r") 'cljr-helm)
