;; wavedrom.el - Wavedrom Integration for Emacs

;; Wavedrom
;;   https://wavedrom.com/
;;   https://github.com/wavedrom/wavedrom
;;
;; Wavedrom-mode:
;;   https://github.com/gmlarumbe/wavedrom-mode
;; Requirements:
;;   'wavedrom-cli'
;; Install/Update 'wavedrom-cli' by using 'npm':
;;   npm i wavedrom-cli -g
;;   npm update -g wavedrom-cli

(use-package wavedrom
  :if (executable-find "wavedrom-cli")
  :mode (("\\.wjson\\'" . wavedrom-mode))
  :config
  (progn
    (setq wavedrom-output-format "svg")
    (setq wavedrom-output-directory ".")))

(provide 'setup-wavedrom)

;; Keybindings:
;; C-c C-c -- wavedrom-compile
;; C-c C-p -- wavedrom-preview-browser
