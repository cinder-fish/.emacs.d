;;; -*- lexical-binding: t -*-

;;; osx-lisp.el --- OSX integration functions / setup

(when (memq window-system '(max ns))

  (defun osx--insert-hash ()
    (interactive)
    (insert "#"))

  (defun osx--open-in-intellij ()
    (interactive)
    (progn
      (shell-command
       (format "idea --line %d %s"
               (line-number-at-pos)
               (buffer-file-name)))
      (start-process-shell-command "Switch to IntelliJ" nil
                                   "osascript -e 'activate application \"IntelliJ IDEA\"'")))

  (global-set-key (kbd "s-O") 'osx--open-in-intellij)

  ;; set a default emoji-font for all frames

  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji")
                                frame 'prepend))))

;;; osx-lisp.el ends here
