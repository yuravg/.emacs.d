;; Time-stamp: <2016-05-17 14:44:02 kmodi>

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
    (add-hook 'tcl-mode-hook #'yura/tcl-set-indentation)))


(provide 'setup-tcl)
