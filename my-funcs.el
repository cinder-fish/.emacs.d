(global-set-key (kbd "C-<return>") 'newline-and-indent)

(load "~/.emacs.d/prelude-move-beginning-of-line")
(require 'prelude-move-beginning-of-line)
(global-set-key [remap move-beginning-of-line]
                'prelude-move-beginning-of-line)

(load "~/.emacs.d/move-lines")
(require 'move-lines)
(move-lines-binding)
