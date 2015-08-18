(setq sml/name-width (quote (10 . 44)))
(setq sml/shorten-directory t)
(setq sml/theme (quote respectful))

(setq mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification " " mode-line-buffer-identification sml/pos-id-separator mode-line-position
     (vc-mode vc-mode)
     sml/pre-modes-separator mode-line-modes mode-line-misc-info mode-line-end-spaces)))

(sml/setup)
