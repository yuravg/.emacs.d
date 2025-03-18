
;; Contents:
;;
;;  JSON

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


(provide 'setup-data-format)
