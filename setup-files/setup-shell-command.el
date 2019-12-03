
;; Shell-command

;; Contents:
;;
;;  Package shell-command
;;    Run current buffer in shell
;;    Package Comint

;;; Package shell-command
(use-package shell-command
  :config
  (progn
    (shell-command-completion-mode)
    ;; allow erase buffer by keybinding
    (put 'erase-buffer 'disabled nil)

    ;; (autoload 'bash-completion-dynamic-complete
    ;;   "bash-completion"
    ;;   "BASH completion hook")
    ;; (add-hook 'shell-dynamic-complete-functions
    ;;   'bash-completion-dynamic-complete)
    ;; (add-hook 'shell-command-complete-functions
    ;;   'bash-completion-dynamic-complete)

    ;; NOTE: this part break down magit (duplicate status instead update)
    ;; Running a Shell Command Asynchronously
    ;; (defadvice erase-buffer (around erase-buffer-noop)
    ;;   "make erase-buffer do nothing")

    (defadvice shell-command (around shell-command-unique-buffer activate compile)
      (if (or current-prefix-arg
              (not (string-match "[ \t]*&[ \t]*\\'" command)) ;; background
              (bufferp output-buffer)
              (stringp output-buffer))
          ad-do-it ;; no behavior change

        ;; else we need to set up buffer
        (let* ((command-buffer-name
                (format "*background: %s*"
                        (substring command 0 (match-beginning 0))))
               (command-buffer (get-buffer command-buffer-name)))

          (when command-buffer
            ;; if the buffer exists, reuse it, or rename it if it's still in use
            (cond ((get-buffer-process command-buffer)
                   (set-buffer command-buffer)
                   (rename-uniquely))
                  ('t
                   (kill-buffer command-buffer))))
          (setq output-buffer command-buffer-name)

          ;; insert command at top of buffer
          (switch-to-buffer-other-window output-buffer)
          (insert "Running command: " command
                  "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n")

          ;; temporarily blow away erase-buffer while doing it, to avoid
          ;; erasing the above
          (ad-activate-regexp "erase-buffer-noop")
          ad-do-it
          (ad-deactivate-regexp "erase-buffer-noop"))))

    ;; http://stackoverflow.com/questions/16676750/windows-emacs-git-bash-and-shell-command
    ;; http://ergoemacs.org/emacs/emacs_env_var_paths.html
    (when (string-equal system-type "windows-nt")
      (setq explicit-shell-file-name "c:/MinGW/msys/1.0/bin/bash.exe"))

    (defalias 'sc 'shell-command)

;;;; Run current buffer in shell
    (defun run-buffer-in-shell ()
      "Run current buffer in existing shell as command: ./<buffer_name>."
      (interactive)
      (save-buffer)
      (let ((fname (file-name-nondirectory (buffer-file-name))))
        (let ((bname (buffer-name)))
          (switch-to-buffer-other-window "*shell*")
          (forward-page)
          (erase-buffer)
          (insert "./" fname)
          (comint-send-input)
          (switch-to-buffer-other-window bname))))
    (key-chord-define-global "CC" #'run-buffer-in-shell)

;;;; Package Comint
    (use-package comint
      :config (put 'erase-buffer 'disabled nil)
      :bind (:map comint-mode-map
             ("C-c C-l" . erase-buffer)
             ("C-j" . comint-send-input)))))


(provide 'setup-shell-command)
