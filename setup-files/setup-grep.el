
;; Emacs *grep* buffer settings file

;; see at ./setup-compilation.el

;;; Varibles
(defvar grep-truncate-lines t
  "Truncate lines for grep buffer.")

;;; Functions
;;;; grep-toggle-truncate-lines
(defun grep-toggle-truncate-lines ()
  "Toggle variable `grep-truncate-lines'."
  (interactive)
  (setq grep-truncate-lines
        (if (bound-and-true-p grep-truncate-lines)
            nil t))
  (message "Truncate line (grep mode), set: '%s'" grep-truncate-lines))

;;; Hydra
(defhydra hydra-grep (:color blue
                      :hint nil)
  "
[grep mode]
_t_runcate line
"
  ("t" grep-toggle-truncate-lines)
  ("q" nil "cancel")
  ("C-g" nil "cancel"))
(with-eval-after-load 'grep
  (bind-key "?" #'hydra-grep/body grep-mode-map))

;;; Hook
(defun yura/grep-mode-hook ()
  "Hook."
  (setq truncate-lines grep-truncate-lines)
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook 'grep-mode-hook #'yura/grep-mode-hook)

(use-package grep
  :bind (:map grep-mode-map
         ("C-j" . compile-goto-error)))


(provide 'setup-grep)
