;; Time-stamp: <2015-03-17 09:28:53 kmodi>

;; Auto complete

(use-package auto-complete-config
  :defer 5
  :config
  (progn
    (setq ac-stop-words (quote ("/" "//" "/*" "//*" "///" "////")))
    (setq ac-ignore-case t) ; ignore case
    (setq ac-use-fuzzy t) ; enable fuzzy auto complete
    (setq ac-trigger-key "TAB")
    (setq ac-disable-faces nil)  ;; enable auto complete between quotation marks

    (defun yura/completion (arg)
      "Execute completion/expand text.

If `auto-complete-mode' enable, will be execute `auto-complete'.
Prefixed with one \\[universal-argument], execute `dabbrev-completion'.
If disable `auto-complete-mode' execute `hippie-expand'."
      (interactive "p")
      (if auto-complete-mode
          (cl-case arg
            (0 (auto-complete))
            (4 (dabbrev-completion))
            (t (auto-complete)))
        (hippie-expand nil)))

    (bind-keys
     :map modi-mode-map
     ("M-/" . yura/completion)  ; by default, 'M-/' is bound to `hippie-expand'
     ("C-M-/" . hippie-expand)) ; by default, 'C-M-/' is bound to `dabbrev-completion'

    ;; http://cx4a.org/software/auto-complete/manual.html#Select_candidates_with_C-n_C-p_only_when_completion_menu_is_displayed
    ;; Use C-n/p instead of arrow keys to select ac options from the ac menu
    (setq ac-use-menu-map t)
    (bind-keys
     :map ac-menu-map
     ("C-n" . ac-next)
     ("C-p" . ac-previous)
     ("C-s" . ac-isearch)
     ("C-j" . ac-complete))

    (ac-config-default)))


(provide 'setup-auto-complete)
