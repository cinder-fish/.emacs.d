(defun split-root-window--base (direction)
  "Split the root window in DIRECTION"
  (split-window (frame-root-window) nil direction))

(defun split-root-window-right ()
  "Split the root window right"
  (interactive)
  (split-root-window--base 'right))

(defun split-root-window-left ()
  "Split the root window left"
  (interactive)
  (split-root-window--base 'left))

(defun split-root-window-below ()
  "Split the root window below"
  (interactive)
  (split-root-window--base 'below))

(defun split-root-window-above ()
  "Split the root window above"
  (interactive)
  (split-root-window--base 'above))
