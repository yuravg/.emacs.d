
;; BitBake

;; https://bitbucket.org/olanilsson/bitbake-modes
(use-package bitbake-mode
  :mode (("\\.conf\\'"     . bitbake-mode)
         ("\\.bb\\'"       . bitbake-mode)
         ("\\.bbappend\\'" . bitbake-mode)
         ("\\.bbclass\\'"  . bitbake-mode))
  :init
  (progn
    (defun my/bitbake-set-comment () (setq comment-start "#"))
    (defun my/bitbake-set-indentation () (setq tab-width 4 indent-tabs-mode nil))
    (add-hook 'bitbake-mode-hook #'my/bitbake-set-comment)
    (add-hook 'bitbake-mode-hook #'my/bitbake-set-indentation)))


(provide 'setup-bitbake)
