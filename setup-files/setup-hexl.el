
;; Hexl

(use-package hexl
  :mode (("\\.bin" . hexl-mode)
         ("\\.dll" . hexl-mode)
         ("\\.pof" . hexl-mode)
         ("\\.sof" . hexl-mode))
  :bind (:map hexl-mode-map
         ("M-j" . hexl-goto-hex-address) ;default binding to `hexl-goto-address'
         ("C-M-j" . hexl-goto-address))
  :init
  (progn
    (defun hexl-do-not-delete-trailing-whitespace ()
      (setq do-not-delete-trailing-whitespace t))
    (add-hook 'hexl-mode-hook #'hexl-do-not-delete-trailing-whitespace)))


(provide 'setup-hexl)
