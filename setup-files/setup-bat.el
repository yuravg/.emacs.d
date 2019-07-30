
;; Setup major mode for editing DOS/Windows scripts

(use-package bat
  :mode (("\\.\\(?:bat\\|com\\)$" . bat-mode))
  :init
  (progn
    (defun yura/bat-set-indentation () (setq tab-width 4 indent-tabs-mode t))
    (add-hook 'bat-mode-hook #'yura/bat-set-indentation)))


(provide 'setup-bat)
