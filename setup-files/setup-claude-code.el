
;; https://github.com/stevemolitor/claude-code.el

;; Contents:
;;
;;  Dependencies
;;  Terminal Backend Selection
;;  IDE Protocol Integration
;;  Claude Code Configuration
;;  TIPS

;;; Dependencies

;; inheritenv: Inherit environment variables from the shell
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;;; Terminal Backend Selection

(defvar claude-code-vterm-available-p (locate-library "vterm")
  "Non-nil if vterm library is available.
When t, vterm will be used as the terminal backend for claude-code.
When nil, falls back to eat terminal emulator.")

;; Configure vterm if available, otherwise use eat as fallback
(if claude-code-vterm-available-p
    (use-package vterm
      :ensure t
      :config
      ;; Extended scrollback buffer for long Claude conversations
      (setq vterm-max-scrollback 50000)

      ;; Auto-cleanup: kill buffer when terminal process exits
      (setq vterm-kill-buffer-on-exit t)

      ;; Unbind C-m (RET) to avoid conflicts with Emacs editing workflow
      ;; This allows more natural Emacs-style editing in vterm buffers
      (define-key vterm-mode-map (kbd "C-m") nil))

  ;; Fallback to eat terminal emulator if vterm is unavailable
  (progn
    (message "Vterm library not found - using eat terminal emulator as fallback")
    (use-package eat
      :ensure t)))

;;; IDE Protocol Integration

;; monet: Provides IDE protocol support for Claude Code
;; Enables features like file operations, code navigation, etc.
(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

;;; Claude Code Configuration

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; Select terminal backend based on availability check
  ;; vterm is preferred for better performance, eat is the fallback
  (setq claude-code-terminal-backend
        (if claude-code-vterm-available-p 'vterm 'eat))

  ;; Display Claude Code window on right side (50% width) instead of bottom
  (setq claude-code-display-window-fn
        (lambda (buffer)
          (display-buffer buffer '((display-buffer-in-side-window)
                                   (side . right)
                                   (window-width . 0.5)))))

  ;; Enable IDE protocol integration via monet
  ;; This allows Claude to interact with your Emacs environment
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  ;; Activate claude-code-mode globally
  (claude-code-mode)

  ;; Define convenient command aliases
  (defalias 'cc 'claude-code "Start Claude Code in a terminal buffer.")
  (defalias 'claude 'claude-code "Start Claude Code in a terminal buffer"))


(provide 'setup-claude-code)

;;; TIPS

;; (1) Getting Started
;;     Start a Claude Code session with:
;;       M-x claude-code    (or the shorter alias: M-x cc)

;; (2) Copy Mode
;;     To select and copy Claude responses, use vterm copy mode:
;;       C-c C-t    Toggle vterm copy mode

;; (3) Key Bindings
;;     In Claude Code buffer (normal mode):
;;       C-j        Insert newline without sending to Claude
;;       RET        Send input to Claude
;;       C-c C-t    Toggle vterm copy mode (for text selection/copying)
;;
;;     In vterm copy mode (C-c C-t enabled):
;;       C-s        Search forward in buffer
;;       C-r        Search backward in buffer
;;       M-w        Copy region to kill ring
;;       C-c C-t    Exit copy mode and return to normal mode
;;
