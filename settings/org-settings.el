(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-mobile-inbox-for-pull "~/org/refile.org")
(setq org-mobile-directory "~/Dropbox/org")

