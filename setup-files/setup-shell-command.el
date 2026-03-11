
;; Shell Command Buffers

;; Contents:
;;
;;    my/shell-run-command
;;    my/shell-exit-and-kill
;;    Run current buffer in shell
;;    Package Comint


;;;; my/shell-run-command
(defun my/shell-run-command (command)
  "Open a new shell buffer and execute COMMAND in it.
  The buffer gets a unique name based on the command."
  (interactive (list (read-shell-command "Shell command: ")))
  (let* ((buf-name (generate-new-buffer-name
                    (format "*shell: %s*" (truncate-string-to-width command 30))))
         (buf (shell buf-name)))
    ;; Wait for shell prompt before sending command
    (set-process-query-on-exit-flag (get-buffer-process buf) nil)
    (comint-send-string (get-buffer-process buf) (concat command "\n"))))

;;;; my/shell-exit-and-kill
(defun my/shell-exit-and-kill ()
  "Send `exit' to the shell process and kill the buffer when it finishes."
  (interactive)
  (when-let* ((proc (get-buffer-process (current-buffer))))
    (set-process-sentinel proc
                          (lambda (process _event)
                            (when (memq (process-status process) '(exit signal))
                              (kill-buffer (process-buffer process)))))
    (comint-send-string proc "exit\n")))
(bind-key "C-c C-q" #'my/shell-exit-and-kill shell-mode-map)

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
         ("C-j" . comint-send-input)))


(provide 'setup-shell-command)
