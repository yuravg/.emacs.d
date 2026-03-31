
;; Contents:
;;
;;  Conf Mode
;;  TOML Mode
;;  JSON

;;; Conf Mode
(use-package conf-mode
  :mode (("\\.conf\\'"    . conf-space-mode)
         ("\\.dat\\'"     . conf-space-mode)
         ("\\.service\\'" . conf-space-mode)
         ("\\.setup.*\\'" . conf-space-mode))
  :config
  (progn
    ;; Unbind "C-c SPC" to release it for `hydra-launch/body'
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
    (defun my/toml-set-indentation ()
      "Customize the indentation for `conf-toml-mode'."
      (setq-local tab-width 2))
    (add-hook 'toml-mode-hook #'my/toml-set-indentation)))


;;; JSON
(use-package js
  :config
  (progn
    (defun my/js-set-indentation ()
      "Customize the indentation for `js-json-mode'."
      (setq tab-width 2
            js-indent-level 2
            indent-tabs-mode nil))
    (add-hook 'js-json-mode-hook #'my/js-set-indentation)

    (use-package plantuml-mode
      :bind
      (("C-c C-c" . my/plantuml-preview-buffer-json))
      :config
      (progn

        (defun my/plantuml-preview-region-json (prefix begin end)
          "Preview diagram from the PlantUML json sources in from BEGIN to END.
Uses the current region when called interactively.
Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
          (interactive "p\nr")
          (plantuml-preview-string prefix (concat "@startjson\n"
                                                  (buffer-substring-no-properties
                                                   begin end)
                                                  "\n@endjson")))

        (defun my/plantuml-preview-buffer-json (prefix)
          "Preview diagram from the PlantUML json sources in the current buffer.
Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame
- else -> new buffer"
          (interactive "p")
          (plantuml-preview-string prefix (concat "@startjson\n"
                                                  (buffer-substring-no-properties
                                                   (point-min) (point-max))
                                                  "\n@endjson")))))))
;; TIPS:

;; (1) C-c C-c - call `plantuml-preview',
;;     prefixed with C-u - open new window
;;     prefixed with C-u C-u - open new frame


(provide 'setup-config-files)
