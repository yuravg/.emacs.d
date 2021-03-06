
;; Text

(use-package text-mode
  :mode (("\\.log\\'" . text-mode)
         ("\\.log\,[0-9]+\\'" . text-mode)
         ("\\.f\\'" . text-mode)         ;I never need to code in Fortran
         ("\\.rpt\\'" . text-mode)
         ("\\.rpt\,[0-9]+\\'" . text-mode))
  :config
  (progn
    ;; http://emacs.stackexchange.com/a/16854/115
    (defun modi/text-mode-comments ()
      "Make text beginning with # look like comments only in `text-mode'."
      (when (equal major-mode 'text-mode)
        (font-lock-add-keywords nil '(("#.+" . font-lock-comment-face)))))
    (add-hook 'text-mode-hook #'modi/text-mode-comments)))


(provide 'setup-text)
