;; UTF-8!
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

;; Setup paths
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(setq settings-dir  (expand-file-name "settings"  user-emacs-directory))
(setq themes-dir    (expand-file-name "themes"    user-emacs-directory))

(add-to-list 'load-path site-lisp-dir)
(let ((default-directory site-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path settings-dir)

;; Separate custom-settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Store backup files in local folder
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat user-emacs-directory "backups")))))

(setq vc-make-backup-files t)

;; Load local proxy settings if present
(load (expand-file-name "proxy.el" user-emacs-directory) t)

;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(setq package-enable-at-startup nil)

(setq my-packages '(helm
                    helm-ls-git
                    helm-ag
                    clojure-mode
                    clj-refactor
                    haskell-mode
                    go-mode
                    ruby-mode
                    markdown-mode
                    yaml-mode
                    log4j-mode
                    paredit
                    magit
                    git-commit
                    cider
                    company
                    rainbow-delimiters
                    flx-ido
                    ido-ubiquitous
                    multiple-cursors
                    expand-region
                    smart-mode-line
                    golden-ratio
                    which-key
                    request
                    hydra
                    org
                    org-trello
                    gnus))

(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (package my-packages)
  (progn
    (when (not (package-installed-p package))
      (package-install package))
    (require package)))

(require 'uniquify)

;; Load custom variables
(load custom-file)

(add-to-list 'custom-theme-load-path themes-dir)
(load-theme 'material t)

;; Load settings & site-lisp
(defun init--load-dir (dir)
  (dolist (file (directory-files dir t "\\w+"))
  (when (file-regular-p file)
    (load file))))

(init--load-dir settings-dir)
(init--load-dir site-lisp-dir)

(setq gc-cons-threshold 20000000)

;; Load local settings
(load "local" t)

(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)


(unless (server-running-p)
  (server-start))
