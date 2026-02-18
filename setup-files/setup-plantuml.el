
;; PlantUML
;; https://plantuml.com/

;; Contents:
;;
;;  Variables
;;  Functions
;;    my/plantuml-set-indentation
;;    my/plantuml-toggle-source-diagram
;;    my/plantuml-export
;;  Key bindings


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

;;; Variables
    ;; Use my local plantuml.jar file
    (setq plantuml-default-exec-mode 'jar)
    ;; Set path to my local plantuml.jar file
    (setq plantuml-jar-path org-plantuml-jar-path)

    (unless (and plantuml-jar-path (file-exists-p plantuml-jar-path))
      (warn "PlantUML JAR not found at %s" plantuml-jar-path))

;;; Functions

;;;; my/plantuml-set-indentation
    (defun my/plantuml-set-indentation ()
      "Customize the indentation for `plantuml-mode'."
      (setq-local tab-width 2
                  plantuml-indent-level 2
                  indent-tabs-mode nil))
    (add-hook 'plantuml-mode-hook #'my/plantuml-set-indentation)

;;;; my/plantuml-toggle-source-diagram
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

;;;; my/plantuml-export
    (defun my/plantuml-export (&optional open)
      "Export current PlantUML buffer to a file.
The output format is determined by `plantuml-output-type' (svg, png, txt, etc.).
If the output file is already open in a buffer, that buffer is auto-reverted.
With a prefix argument (\\[universal-argument]), open the output file after export."
      (interactive "P")
      (unless (buffer-file-name)
        (user-error "Buffer is not visiting a file"))
      (let* ((input-file (buffer-file-name))
             (jar-path   plantuml-jar-path)
             (out-type   plantuml-output-type)
             (out-ext    (cond ((string= out-type "txt")  "atxt")
                               ((string= out-type "utxt") "utxt")
                               (t out-type)))
             (out-file   (concat (file-name-sans-extension input-file) "." out-ext))
             (err-buf    (get-buffer-create "*plantuml-export*")))
        (unless (file-exists-p jar-path)
          (user-error "PlantUML JAR not found at %s" jar-path))
        (save-buffer)
        (with-current-buffer err-buf (erase-buffer))
        (message "Exporting %s to %s..." (file-name-nondirectory input-file) out-type)
        (let ((exit-code (call-process "java" nil err-buf nil
                                       "-jar" jar-path
                                       (concat "-t" out-type)
                                       input-file)))
          (if (= exit-code 0)
              (if (file-exists-p out-file)
                  (progn
                    ;; Auto-revert if the output file is already open
                    (let ((buf (find-buffer-visiting out-file)))
                      (when buf
                        (with-current-buffer buf (revert-buffer t t t))))
                    (message "Exported to %s" (abbreviate-file-name out-file))
                    (when open (find-file out-file)))
                (message "Command succeeded but %s not found" out-ext))
            (display-buffer err-buf)
            (message "Export failed (exit code %d). See *plantuml-export* buffer."
                     exit-code)))))

;;; Key bindings
    (bind-keys
     ("C-c C-h" . my/plantuml-toggle-source-diagram)
     ("C-c C-b" . plantuml-preview-region)
     ("C-c C-e" . my/plantuml-export))))


(provide 'setup-plantuml)
