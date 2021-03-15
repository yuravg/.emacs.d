
;; isend-mode

;; https://github.com/ffevotte/isend-mode.el

(use-package isend-mode
  :bind
  (:map modi-mode-map
   ("C-x C-i" . isend-associate)
   ("C-x i"   . isend-send-buffer)
   ("C-x M-i" . isend-display-buffer)))

;TODO: how interact with custom of isend from setup-perl

(provide 'setup-isend)

;; TIPS
;; isend-associate
;; isend-send           <C-return>
;; isend-send-buffer
;; isend-display-buffer
