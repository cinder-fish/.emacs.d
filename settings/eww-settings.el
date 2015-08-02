(defadvice eww-render (after set-eww-buffer-name activate)
  (rename-buffer (concat "*eww-" (or eww-current-title
				     (if (string-match "://" eww-current-url)
					 (substring eww-current-url (match-beginning 0))
				       eww-current-url)) "*") t))



