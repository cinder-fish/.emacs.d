(setq
 ;; Disable autoloads
 site-run-file nil
 inhibit-default-init t

 ;; No GC during init
 gc-cons-threshold most-positive-fixnum

 ;; Ensure native-comp optimisations
 comp-speed 2
 native-comp-speed 2)

;; Reset GC to 20M
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024))))
