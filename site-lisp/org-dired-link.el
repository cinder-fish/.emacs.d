;;; org-dired-link.el - Support 'dired' links to directories in Org

(require 'org)

(org-add-link-type "dired" 'org-dired-open)
(add-to-list 'org-link-frame-setup '(dired . dired-other-window))
(add-hook 'org-store-link-functions 'org-dired-store-link)

(defun org-dired-open (path)
  "Visit the path in a new dired buffer.
   If path is a file visit the parent directory."
  (funcall (cdr (assq 'dired org-link-frame-setup))
	   (file-name-directory path)))

(defun org-dired-store-link ()
  "Store a link to a dired buffer."
  (when (memq major-mode '(dired-mode))
    (let* ((path default-directory)
	   (link (concat "dired:" path)))
      (org-store-link-props
       :type "dired"
       :link link
       :description path))))

(provide 'org-dired-link)

;;; org-dired-link.el ends here
