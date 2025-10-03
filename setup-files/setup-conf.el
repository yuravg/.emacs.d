;; Time-stamp: <2017-08-15 12:05:32 kmodi>

;; Contents:
;;
;;  Conf Mode
;;  TOML Mode


;;; Conf Mode
(use-package conf-mode
  :mode (("\\.conf\\'"    . conf-space-mode)
         ("\\.dat\\'"     . conf-space-mode)
         ("\\.service\\'" . conf-space-mode)
         ("\\.setup.*\\'" . conf-space-mode))
  :config
  (progn
    ;Unbind "C-c SPC" to release it for `hydra-launch/body'
    (unbind-key "C-c SPC" conf-mode-map) ;default: `conf-space-keywords'

    (defun modi/conf-quote-normal ()
      "Enable `conf-quote-normal' for *.setup files."
      (when-let* ((fname (buffer-file-name))
                  (enable-conf-quote-normal (string-match-p "\\.setup.*" fname)))
        ;; Set the syntax of ' and " to punctuation.
        (conf-quote-normal nil)))
    (add-hook 'conf-space-mode-hook #'modi/conf-quote-normal)))


;;; TOML Mode
(use-package toml-mode
  :ensure t
  :mode (("\\.toml\\'"        . toml-mode)
         ("/Cargo\\.lock\\'"  . toml-mode)
         ("/Pipfile\\'"      . toml-mode)
         ("/poetry\\.lock\\'" . toml-mode))

  :config
  (progn
    (defun yura/toml-set-indentation ()
      "Customize the indentation for `conf-toml-mode'."
      (setq-local tab-width 2))
    (add-hook 'toml-mode-hook #'yura/toml-set-indentation)))


(provide 'setup-conf)
