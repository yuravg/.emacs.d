
;; Autopair

;; https://www.emacswiki.org/emacs/AutoPairs
;; https://github.com/joaotavora/autopair
;; TODO: https://www.emacswiki.org/emacs/ParEdit

(use-package autopair
  :config
  (progn

    ;; Disable autopair blinks matching delimiters
    (setq autopair-blink nil)

    (defun turn-on-autopair-mode () (autopair-mode 1))

    (defvar yura/autopair-mode-hooks yura/major-modes-hooks
      "List of hooks of major modes in which `autopair-mode' should be enabled.")

    (dolist (hook yura/autopair-mode-hooks)
      (add-hook hook #'turn-on-autopair-mode))

    (defalias 'ap 'autopair-mode)

    (add-hook 'c++-mode-hook
              (lambda ()
                (push ?{
                      (getf autopair-dont-pair :comment))
                (push '(?< . ?>)
                      (getf autopair-extra-pairs :comment))))

    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (push '(?` . ?')
                      (getf autopair-extra-pairs :comment))
                (push '(?` . ?')
                      (getf autopair-extra-pairs :string))))

    (add-hook 'makefile-mode-hook
              (lambda ()
                (push '(?< . ?>)
                      (getf autopair-extra-pairs :comment))
                (push '(?' . ?')
                      (getf autopair-extra-pairs :comment))))))


(provide 'setup-autopair)
