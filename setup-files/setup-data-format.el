
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
    (add-hook 'js-json-mode-hook #'my/js-set-indentation)))


(provide 'setup-data-format)
