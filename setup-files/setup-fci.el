;; Time-stamp: <2017-06-27 17:48:45 kmodi>

;; Fill Column Indicator
;; http://www.emacswiki.org/FillColumnIndicator

(use-package fill-column-indicator
  :if (not (bound-and-true-p disable-pkg-fci))
  :config
  (progn
    ;; Set global default value for the local var `fci-handle-truncate-lines'
    (setq-default fci-handle-truncate-lines t) ; Truncate lines in fci mode
    ;; (setq-default fci-handle-truncate-lines nil) ; Do not truncate lines in fci mode

    (setq fci-rule-width 1)
    (setq-default fci-rule-use-dashes t)
    (setq-default fci-dash-pattern 1.0)
    (setq-default fci-rule-color "grey85")
    (setq-default fci-rule-column 96)

    (defconst modi/fci-mode-hooks '(verilog-mode-hook
                                    emacs-lisp-mode-hook
                                    python-mode-hook
                                    sh-mode-hook
                                    ;; org-mode-hook
                                    org-src-mode-hook
                                    cperl-mode-hook
                                    makefile-mode-hook
                                    text-mode-hook
                                    fundamental-mode
                                    tcl-mode-hook
                                    java-mode-hook
                                    c-mode-hook
                                    c++-mode-hook
                                    rust-mode-hook
                                    bat-mode-hook
                                    powershell-mode-hook
                                    bitbake-mode-hook
                                    js-mode-hook
                                    nim-mode-hook
                                    d-mode-hook)
      "List of hooks of major modes in which fci mode should be enabled.")

    (defun modi/turn-on-fci-mode ()
      "Turn on fci-mode only for specific modes.
    As truncation is enabled only in fci-mode, truncation will be activated
    only in the below modes"
      (interactive)
      (dolist (hook modi/fci-mode-hooks)
        (add-hook hook #'fci-mode)))

    (defun modi/turn-off-fci-mode ()
      "Turn off fci-mode only for specific modes."
      (interactive)
      (dolist (hook modi/fci-mode-hooks)
        (remove-hook hook #'fci-mode)))

    (modi/turn-on-fci-mode)

    ;; Turn off fci-mode when popups are activated
    ;; https://github.com/alpaker/Fill-Column-Indicator/issues/21#issuecomment-6959718
    (with-eval-after-load 'popup
      (defvar modi/fci-mode-original-state nil
        "Variable to store the initial state of `fci-mode'.")

      (defun modi/popup--fci-disable-temporarily (&rest args)
        "Suspend `fci-mode' for time being."
        (setq-local modi/fci-mode-original-state fci-mode)
        (when fci-mode
          (fci-mode -1)))
      (advice-add 'popup-create :before #'modi/popup--fci-disable-temporarily)

      (defun modi/popup--fci-restore (&rest args)
        "Restore `fci-mode' when all popups have closed"
        (when (and modi/fci-mode-original-state
                   (null popup-instances))
          (setq-local modi/fci-mode-original-state nil)
          (fci-mode 1)))
      (advice-add 'popup-delete :after #'modi/popup--fci-restore))

    ;; `fci-mode' needs to be disabled/enabled around the
    ;; `shell-command-on-region' command too. Because if that is not done and if
    ;; `C-u M-x shell-command-on-region' is called, the inserted text is pasted
    ;; to the right of the fci. It's just a visual glitch due to the fci
    ;; overlays. But to fix it, you would need to refresh the display otherwise,
    ;; each time that happened.
    (defun modi/shell--fci-disable-temporarily (orig-fun &rest args)
      "Disable `fci-mode' before calling ORIG-FUN; re-enable afterwards."
      (let ((fci-was-initially-on (when fci-mode
                                    (prog1
                                        fci-mode
                                      (fci-mode -1)))))
        (prog1
            (apply orig-fun args)
          (when fci-was-initially-on
            (fci-mode 1)))))
    (advice-add 'shell-command-on-region :around #'modi/shell--fci-disable-temporarily)

    (defun modi/fci-redraw-frame-all-buffers ()
      "Redraw the fill-column rule in all buffers on the selected frame.
Running this function after changing themes updates the fci rule color in
all the buffers."
      (interactive)
      (let ((bufs (delete-dups (buffer-list (selected-frame)))))
        (dolist (buf bufs)
          (with-current-buffer buf
            (when fci-mode
              (fci-delete-unneeded)
              (fci-make-overlay-strings)
              (fci-update-all-windows t))))))))


(provide 'setup-fci)
