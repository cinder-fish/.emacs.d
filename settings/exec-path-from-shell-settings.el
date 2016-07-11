;; On OSX make PATHs like shell
(require 'exec-path-from-shell)
(when (memq window-system '(max ns))
  (exec-path-from-shell-initialize))
