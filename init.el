(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Load local proxy settings if present
(if (file-readable-p "~/.emacs.d/proxy.el")
    (load "~/.emacs.d/proxy"))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(setq package-enable-at-startup nil)

(setq my-packages '(clojure-mode
		    clj-refactor
		    scala-mode
		    python-mode
		    ruby-mode
		    protobuf-mode
		    markdown-mode
		    log4j-mode
		    paredit
		    magit
		    git-commit-mode
		    auto-complete
		    nrepl
		    ac-nrepl
		    volatile-highlights
		    rainbow-delimiters
		    flx-ido
		    dired+
		    multiple-cursors
		    expand-region
		    zenburn-theme
		    solarized-theme
		    org
		    gnus
		    w3m))

(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (package my-packages)
  (progn 
    (when (not (package-installed-p package))
      (package-install package))
    (require package)))


(require 'uniquify)

(volatile-highlights-mode t)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

(setq gc-cons-threshold 20000000)

;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
; (add-hook 'clojure-mode-hook 'auto-complete-mode)

(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-to-list 'ac-modes 'nrepl-mode)

(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

;; Expand-region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

;; Additional elisp functions
(load "~/.emacs.d/my-funcs")

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-mobile-inbox-for-pull "~/org/refile.org")
(setq org-mobile-directory "~/Dropbox/org")

;; Load local settings
(if (file-readable-p "~/.emacs.d/local.el")
    (load "~/.emacs.d/local"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("73b835431bdbc4e83a3b176a38ebb740fbac78aa2635e1d4827b3c8211e0bc99" "7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "62b86b142b243071b5adb4d48a0ab89aefd3cf79ee3adc0bb297ea873b36d23f" "605080e40891cc991f53d3d9c79b427d18497d973a44fd12a86d2360429a6a3d" "865d6cb994f89c13b2d7e5961df4eabeea12494583c240c8fe9a788d0f4ee12c" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" default)))
 '(desktop-save-mode t)
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^#.*#$")
 '(org-agenda-files (quote ("~/org/refile.org" "~/org/main.org")))
 '(org-completion-use-ido t)
 '(org-default-notes-file "~/org/refile.org")
 '(org-log-done (quote time))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)

(server-start)
