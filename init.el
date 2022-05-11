;; init.el

;; defer package control to config
(setq package-enable-at-startup nil)

;; backup to folders in .emacs.d
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat user-emacs-directory "backups")))))
(let ((autosave-directory
       (expand-file-name (concat user-emacs-directory "autosaves"))))
  (make-directory autosave-directory t)
  (setq auto-save-file-name-transforms
        `((".*" ,(concat autosave-directory "/\\1") t))))

;; customize variables in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; load config after init
(add-hook 'after-init-hook
          `(lambda ()
             (setq package-archives
                   '(("gnu" . "https://elpa.gnu.org/packages/")
		     ("melpa-stable" . "https://stable.melpa.org/packages/")
                     ("melpa" . "https://melpa.org/packages/"))
                   package-archive-priorities
                   '(("melpa-stable" . 10)
                     ("gnu"          . 5)
                     ("melpa"        . 0))
                   package-pinned-packages
                   '((use-package . "melpa")))

	     (package-initialize)

             (unless package-archive-contents
               (package-refresh-contents))

	     ;; install org from org archive if we only have built-in version
             (unless (string-match-p
		      (expand-file-name "elpa" user-emacs-directory)
		      (org-version nil t))
	       (package-download-transaction
		(delq nil (mapcar (lambda (x)
				    (and (equal "org" (package-desc-archive x)) x))
				  (cdr (assoc 'org package-archive-contents))))))

             (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
             (load custom-file t)
             ))
