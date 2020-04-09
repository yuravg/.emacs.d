;; Time-stamp: <2016-05-19 22:32:31 kmodi>

;; Contents:
;;
;;  Indent Guide
;;  Highlight-indent-guides

;;; Indent Guide
;; https://github.com/zk-phi/indent-guide

(use-package indent-guide
  :commands (modi/turn-on-indent-guide)
  :config
  (progn
    (setq indent-guide-recursive t)
    (setq indent-guide-char " ")
    (set-face-foreground 'indent-guide-face "grey90")
    (set-face-background 'indent-guide-face "gray90")

    (defvar modi/indent-guide-mode-hooks '(verilog-mode-hook
                                           emacs-lisp-mode-hook
                                           python-mode-hook
                                           sh-mode-hook
                                           cperl-mode-hook)
      "List of hooks of major modes in which indent-guide-mode should be enabled.")

    (defun modi/turn-on-indent-guide ()
      "Turn on indent-guide-mode only for specific modes."
      (interactive)
      (dolist (hook modi/indent-guide-mode-hooks)
        (add-hook hook #'indent-guide-mode)))

    (defun modi/turn-off-indent-guide ()
      "Turn off indent-guide-mode only for specific modes."
      (interactive)
      (indent-guide-global-mode -1)
      (dolist (hook modi/indent-guide-mode-hooks)
        (remove-hook hook #'indent-guide-mode)))

    (indent-guide-global-mode -1)))


;;; Highlight-indent-guides
;; https://github.com/DarthFennec/highlight-indent-guides

(use-package highlight-indent-guides
  :config
  (progn
    (setq highlight-indent-guides-method 'column)
    (set-face-background 'highlight-indent-guides-odd-face "gray92")
    (set-face-background 'highlight-indent-guides-even-face "gray90")

    (defalias 'ig 'highlight-indent-guides-mode)))


(provide 'setup-indent-guide)
