
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

    ;; JSON
    (use-package json-mode
      :bind
      (("C-c C-c" . plantuml-preview)))))

;; TIPS:

;; (1) C-c C-c - call `plantuml-preview',
;;     prefixed with C-u - open new window
;;     prefixed with C-u C-u - open new frame


(provide 'setup-plantuml)
