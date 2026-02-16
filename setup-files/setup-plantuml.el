
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
  :custom
  (plantuml-default-exec-mode 'jar "Use local plantuml.jar")
  (plantuml-jar-path org-plantuml-jar-path "Reuse org-babel's JAR path")
  :bind
  (:map plantuml-mode-map
   ("C-c C-h" . my/plantuml-toggle-source-diagram))
  :hook
  (plantuml-mode . my/plantuml-set-indentation)
  :config
  (progn
    (unless (and plantuml-jar-path (file-exists-p plantuml-jar-path))
      (warn "PlantUML JAR not found at %s" plantuml-jar-path))

    (defun my/plantuml-toggle-source-diagram ()
      "Toggle between a PlantUML source file and its rendered output.
Switch from source (.puml, .pu, .wsd, .plantuml, .iuml) to the
corresponding .svg or .png, and vice versa."
      (interactive)
      (unless (buffer-file-name)
        (user-error "Buffer is not visiting a file"))
      (let* ((file (buffer-file-name))
             (ext  (file-name-extension file))
             (base (file-name-sans-extension file))
             (source-exts '("puml" "pu" "wsd" "plantuml" "iuml"))
             (target
              (cond
               ((member ext source-exts)
                (or (let ((svg (concat base ".svg")))
                      (when (file-exists-p svg) svg))
                    (let ((png (concat base ".png")))
                      (when (file-exists-p png) png))))
               ((member ext '("svg" "png"))
                (seq-find #'file-exists-p
                          (mapcar (lambda (e) (concat base "." e))
                                  source-exts))))))
        (if target
            (find-file target)
          (message "No corresponding file found for %s"
                   (file-name-nondirectory file)))))

    (defun my/plantuml-set-indentation ()
      "Customize the indentation for `plantuml-mode'."
      (setq-local tab-width 2
                  plantuml-indent-level 2
                  indent-tabs-mode nil))))


(provide 'setup-plantuml)
