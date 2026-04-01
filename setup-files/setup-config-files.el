
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
    (add-hook 'toml-mode-hook #'my/toml-set-indentation)

    ;; Dependencies:
    ;;   pip install toml
    (defun my/toml-to-json-string (toml-string)
      "Convert TOML-STRING to JSON using Python.
Requires Python 3.11+ (tomllib) or the tomli package."
      (let ((python (if (eq system-type 'windows-nt)
                        (executable-find "python")    ; Only this path has necessary 'toml' on my Windows OS
                      (or (executable-find "python3")
                          (executable-find "python")))))
        (unless python
          (user-error "Python not found; install Python 3.11+ for TOML conversion"))
        (with-temp-buffer
          (insert toml-string)
          (let ((exit-code
                 (call-process-region
                  (point-min) (point-max) python
                  t t nil  ;delete input, output to current buffer, no stderr buffer
                  "-c"
                  ;; Single-line script: newlines in `-c' args
                  ;; break on Windows (CreateProcess flattens them).
                  (concat "import sys, json; "
                          "tomllib = __import__('tomllib') if sys.version_info >= (3, 11) else __import__('tomli'); "
                          "print(json.dumps(tomllib.loads(sys.stdin.read()), indent=2))"))))
            (unless (eq exit-code 0)
              (user-error "TOML to JSON conversion failed:\n%s"
                          (buffer-string)))
            (buffer-string)))))

    (use-package plantuml-mode
      :bind
      (:map toml-mode-map
       ("C-c C-c" . my/plantuml-preview-toml-buffer))
      :config
      (progn
        (defun my/plantuml-preview-toml-region (prefix begin end)
          "Preview PlantUML JSON diagram from TOML sources in region BEGIN to END.
Converts TOML to JSON via Python, then previews with PlantUML.
Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame
- else -> new buffer"
          (interactive "p\nr")
          (let ((json (my/toml-to-json-string
                       (buffer-substring-no-properties begin end))))
            (plantuml-preview-string prefix (concat "@startjson\n"
                                                    json
                                                    "\n@endjson"))))

        (defun my/plantuml-preview-toml-buffer (prefix)
          "Preview PlantUML JSON diagram from TOML sources in buffer or region.
If a region is active, convert only the selected TOML to JSON.
Otherwise, convert the entire buffer.
Uses prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame
- else -> new buffer"
          (interactive "p")
          (if (use-region-p)
              (my/plantuml-preview-toml-region
               prefix (region-beginning) (region-end))
            (my/plantuml-preview-toml-region
             prefix (point-min) (point-max))))))))


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
      (("C-c C-c" . my/plantuml-preview-json-buffer))
      :config
      (progn

        (defun my/plantuml-preview-json-region (prefix begin end)
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

        (defun my/plantuml-preview-json-buffer (prefix)
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
