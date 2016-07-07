(require 'dash)
(defun clojure-settings/add-hooks (hooks fns)
  (--each (-table-flat 'cons hooks fns)
    (add-hook (car it) (cdr it))))

(clojure-settings/add-hooks
 '(clojure-mode-hook
   clojurescript-mode-hook
   cider-mode-hook
   cider-repl-mode-hook)
 '(paredit-mode
   rainbow-delimiters-mode
   eldoc-mode))

(setq nrepl-hide-special-buffers t)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-show-error-buffer nil)
(setq cider-repl-use-pretty-printing t)

(setq cljr-warn-on-eval nil)

(define-key clojure-mode-map (kbd "C-c r") 'cljr-helm)
