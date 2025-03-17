
;; PlantUML

;; https://plantuml.com/

(use-package plantuml-mode
  ;; Default: *.plantuml, *.pum, *.plu
  :mode
  (("\\.wsd\\'"      . plantuml-mode)
   ("\\.pu\\'"       . plantuml-mode)
   ("\\.puml\\'"     . plantuml-mode)
   ("\\.plantuml\\'" . plantuml-mode)
   ("\\.iuml\\'"     . plantuml-mode))
  :after
  (org ob ob-plantuml)
  :config
  (progn
    ;; Use my local plantuml.jar file
    (setq plantuml-default-exec-mode 'jar)
    ;; Set path to my local plantuml.jar file
    (setq plantuml-jar-path org-plantuml-jar-path)
    (defun yura/plantuml-set-indentaion ()
      "Customize the indentation for `plantuml-mode'."
      (setq tab-width 2
            plantuml-indent-level 2
            indent-tabs-mode nil))
    (add-hook 'plantuml-mode-hook #'yura/plantuml-set-indentaion)


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
                                              "\n@endjson")))

    ;; JSON
    (use-package json-mode
      :bind
      (("C-c C-c" . plantuml-preview)))))

;; TIPS:

;; (1) C-c C-c - call `plantuml-preview',
;;     prefixed with C-u - open new window
;;     prefixed with C-u C-u - open new frame


(provide 'setup-plantuml)
