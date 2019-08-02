;; Time-stamp: <2018-05-17 14:44:23 kmodi>

;; Flycheck
;; https://github.com/flycheck/flycheck
;; http://www.flycheck.org/en/latest/index.html

(use-package flycheck
  :defer t
  :config
  (progn
    (setq flycheck-mode-line-prefix "Ω")
    (defconst modi/flycheck-mode-hooks '(python-mode-hook
                                         emacs-lisp-mode-hook
                                         sh-mode-hook
                                         c++-mode-hook
                                         c-mode-hook
                                         ;; nim-mode-hook
                                         )
      "List of hooks of major modes in which flycheck mode should be enabled.")

    (defun modi/turn-on-flycheck-mode ()
      "Turn on flycheck-mode for the modes in `modi/flycheck-mode-hooks'."
      (interactive)
      (dolist (hook modi/flycheck-mode-hooks)
        (add-hook hook #'flycheck-mode)))

    (defun modi/turn-off-flycheck-mode ()
      "Turn off flycheck-mode for the modes in `modi/flycheck-mode-hooks'."
      (interactive)
      (dolist (hook modi/flycheck-mode-hooks)
        (remove-hook hook #'flycheck-mode)))

    (modi/turn-on-flycheck-mode)

    ;; Disable checkers
    ;; http://www.flycheck.org/en/latest/user/syntax-checkers.html
    ;; (setq-default flycheck-disabled-checkers '(sh-posix-dash))
    ;; (if (eq system-type 'windows-nt)
    ;;     (setq-default flycheck-disabled-checkers
    ;;                   (append flycheck-disabled-checkers '(verilog-verilator))))

    ;;; Flycheck Variables
    (setq flycheck-flake8-maximum-line-length 99)
    (setq flycheck-checker-error-threshold 800)

    ;; Enable C++ exceptions
    (add-to-list 'flycheck-clang-args "-fcxx-exceptions")

    ;;; Python checkers
    (use-package pylint
      :config
      ;; Messages:
      ;; http://stackoverflow.com/questions/4341746/how-do-i-disable-a-pylint-warning
      ;; http://pylint-messages.wikidot.com/all-messages
      ;; http://pylint.readthedocs.io/en/1.6.0/features.html
      (progn
        (autoload 'pylint "pylint")
        (add-hook 'python-mode-hook #'pylint-add-menu-items)
        (add-hook 'python-mode-hook #'pylint-add-key-bindings)

        ;; Enable pylint with flycheck:
        (add-to-list 'flycheck-checkers 'python-pylint)
        ;; simultaneous launch checkers:
        ;; https://github.com/flycheck/flycheck/issues/186
        (flycheck-add-next-checker 'python-flake8 'python-pylint)))))


(provide 'setup-flycheck)

;;; Notes
;; flycheck keymap prefix: "C-c !"
;; disable checker: flycheck-disable-checker
;; enable  checker: C-u flycheck-disable-checker
