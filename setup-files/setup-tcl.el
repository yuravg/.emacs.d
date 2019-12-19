;; Time-stamp: <2019-08-22 12:09:05 kmodi>

;; Tcl

(use-package tcl
  :mode (("\\.tcl\\'"   . tcl-mode)
         ("\\.tcons\\'" . tcl-mode)
         ("\\.svcf\\'"  . tcl-mode)
         ("\\.cer\\'"   . tcl-mode)
         ("\\.qsf\\'"   . tcl-mode)
         ("\\.qpf\\'"   . tcl-mode)
         ("\\.qip\\'"   . tcl-mode)
         ("\\.sdc\\'"   . tcl-mode)
         ("\\.do\\'"    . tcl-mode)
         ("\\.color\\'" . tcl-mode))
  :config
  (progn
    (defun yura/tcl-set-indentation ()
      "Customize the indentation for `tcl-mode'."
      (setq tab-width 4
            tcl-indent-level 4
            indent-tabs-mode nil))
    (add-hook 'tcl-mode-hook #'yura/tcl-set-indentation)

    ;; SDC - Synopsys Design Constraints
    (defun sdc-command-refactoring ()
      "Refactoring the SDC command in the selected region or in the current line.

If a region is selected then this region will be refactored,
otherwise the current line will be refactored.\n
Before:
sdc_command -arg0 -arg1 -arg2 value\n
After:
sdc_command \\
   -arg0 \\
   -arg1 \\
   -arg2 value"
      (interactive)
      (let ((beg (if (use-region-p) (region-beginning) (line-beginning-position)))
            (end (if (use-region-p) (region-end) (line-end-position))))
        (save-restriction
          (narrow-to-region beg end)
          (save-excursion
            (goto-char beg)
            (while (re-search-forward "\\( \\)\\(-\\)" (point-max) :noerror)
              (replace-match "\\1\\\\\n\\2"))
            (indent-region (point-min) (point-max))
            (message "Finished refactoring SDC commmand.")))))

    ;; Intel Quartus tool automatically removes the newline,
    ;; therefore it better not to add newline.
    (defun tcl-qsf-do-not-add-newlines ()
      "Set `require-final-newline' local variable to nil for qsf-files."
      (when (and (derived-mode-p 'tcl-mode)
                 (string= (file-name-extension buffer-file-name) "qsf"))
        (setq-local require-final-newline nil)))

    (add-hook 'tcl-mode-hook #'tcl-qsf-do-not-add-newlines)))


(provide 'setup-tcl)
