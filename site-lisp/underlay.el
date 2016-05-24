;;; -*- lexical-binding: t -*-

;;; underlay.el --- Some utility overlay functions

(require 'dash)

(defvar underlay-view-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "\t" 'underlay--toggle-invis-next-overlay)
      (define-key map "[tab]" 'underlay--toggle-invis-next-overlay)
      (define-key map "n" 'forward-line)
      (define-key map "p" 'previous-line)
      (define-key map "q" 'delete-window)))
  "Keymap for `underlay-view-mode'.")

(define-minor-mode underlay-view-mode
  "Underlay View mode.
A minor mode for browsing underlay overlays."
  nil
  " underlay"
  underlay-view-mode-map)

(defun underlay-write-overlay (text &optional props)
  "Create an underlay overlay containing TEXT, with properties in
  alist PROPS, at point

will also set a property 'underlay--overlay-p' to t"
  (let ((beginning (point))
        new-overlay)
    (insert text)
    (setq new-overlay (make-overlay beginning (point)))
    (overlay-put new-overlay 'underlay--overlay-p t)
    (when props
      (-each props (lambda (prop) (overlay-put new-overlay (car prop) (cdr prop)))))))

(defun underlay--toggle-invis-next-overlay ()
  (interactive)
  (let ((overlay (-first (lambda (ov) (overlay-get ov 'underlay--overlay-p))
                         (-concat (overlays-at (point))
                                  (overlays-at (next-overlay-change (point)))))))
    (when overlay
      (if (overlay-get overlay 'invisible)
          (overlay-put overlay 'invisible nil)
        (progn
          (overlay-put overlay 'invisible t)
          (goto-char (overlay-start overlay))
          (move-beginning-of-line 0))))))

(provide 'underlay)

;;; underlay.el ends here
