
;; PlantUML

;; https://plantuml.com/

(use-package plantuml-mode
  :after
  (org ob ob-plantuml)
  :config
  (progn
    ;; Use my local plantuml.jar file
    (setq plantuml-default-exec-mode 'jar)
    ;; Set path to my local plantuml.jar file
    (setq plantuml-jar-path org-plantuml-jar-path)))

;; TIPS:

;; (1) C-c C-c - call `plantuml-preview',
;;     prefixed with C-u - open new window
;;     prefixed with C-u C-u - open new frame


(provide 'setup-plantuml)
