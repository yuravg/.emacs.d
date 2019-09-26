
;; Rst mode

(use-package rst
  :config
  (progn
    (unbind-key "C-c C-n" rst-mode-map) ;default: `rst-deprecated-backward-section'
    (unbind-key "C-c C-p" rst-mode-map) ;default: `rst-deprecated-forward-section'
    (bind-keys
     :map rst-mode-map
     ("C-c C-n" . rst-forward-section)
     ("C-c C-p" . rst-backward-section)
     ("M-n" . rst-forward-section)
     ("M-p" . rst-backward-section))))


(provide 'setup-rst)
