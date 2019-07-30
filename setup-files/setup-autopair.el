
;; Autopair

;; https://www.emacswiki.org/emacs/AutoPairs
;; https://github.com/joaotavora/autopair
;; TODO: https://www.emacswiki.org/emacs/ParEdit

(use-package autopair
  :config
  (progn
    (defvar autopair-modes '(verilog-mode
                             emacs-lisp-mode
                             python-mode
                             sh-mode
                             org-mode
                             org-src-mode
                             cperl-mode
                             makefile-mode
                             makefile-gmake-mode
                             fundamental-mode
                             text-mode
                             tcl-mode
                             c-mode
                             c++-mode))
    (defun turn-on-autopair-mode () (autopair-mode 1))
    (dolist (mode autopair-modes)
      (add-hook (intern (concat (symbol-name mode) "-hook")) #'turn-on-autopair-mode))

    (defalias 'ap 'autopair-mode)

    (add-hook 'c++-mode-hook
              #'(lambda ()
                  (push ?{
                        (getf autopair-dont-pair :comment))
                  (push '(?< . ?>)
                        (getf autopair-extra-pairs :comment))))

    (add-hook 'emacs-lisp-mode-hook
              #'(lambda ()
                  (push '(?` . ?')
                        (getf autopair-extra-pairs :comment))
                  (push '(?` . ?')
                        (getf autopair-extra-pairs :string))))

    (add-hook 'makefile-mode-hook
              #'(lambda ()
                  (push '(?< . ?>)
                        (getf autopair-extra-pairs :comment))
                  (push '(?' . ?')
                        (getf autopair-extra-pairs :comment))))))


(provide 'setup-autopair)
