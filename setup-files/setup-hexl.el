
;; Hexl

(use-package hexl-mode
  :mode (("\\.bin" . hexl-mode)
         ("\\.dll" . hexl-mode)
         ("\\.pof" . hexl-mode)
         ("\\.sof" . hexl-mode))
  :init
  (progn
    (defun hexl-do-not-delete-trailing-whitespace ()
      (setq do-not-delete-trailing-whitespace t))
    (add-hook 'hexl-mode-hook #'hexl-do-not-delete-trailing-whitespace)))


(provide 'setup-hexl)
