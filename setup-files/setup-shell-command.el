
;; Shell-command

;; Contents:
;;
;;    Run current buffer in shell
;;    Package Comint

(use-package simple
  :config
  (progn
    (defalias 'sc 'shell-command)))

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
