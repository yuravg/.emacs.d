;; Arduino

;; https://www.arduino.cc/

(use-package arduino-mode
  :config
  (progn
    (defun yura/set-c++-comment-style ()
      "Set C++ comment style."
      (setq comment-start "//")
      (setq comment-end ""))
    (add-hook 'arduino-mode-hook #'yura/set-c++-comment-style)

    (with-eval-after-load 'google-c-style
      (add-hook 'arduino-mode-hook #'google-set-c-style))))
;; |-----------------------+-----------|
;; | Function              | Keymap    |
;; |-----------------------+-----------|
;; | Build                 | C-c C-v   |
;; | Upload                | C-c C-c   |
;; |-----------------------+-----------|

(use-package arduino-cli-mode
  :ensure t
  :hook arduino-mode
  :custom
  (arduino-cli-warnings 'all)
  (arduino-cli-verify t))
;; |-----------------------+-----------|
;; | Function              | Keymap    |
;; |-----------------------+-----------|
;; | Compile               | C-c C-a c |
;; | Upload                | C-c C-a u |
;; | Compile and Upload    | C-c C-a b |
;; | List Connected Boards | C-c C-a l |
;; | Create new sketch     | C-c C-a n |
;; | Install a Library     | C-c C-a i |
;; | Uninstall a Library   | C-c C-a u |
;; |-----------------------+-----------|


(provide 'setup-arduino)
