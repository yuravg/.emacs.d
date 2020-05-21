
;; Emacs *compilation* buffer settings file

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation.html
;; http://stackoverflow.com/questions/1292936/line-wrapping-within-emacs-compilation-buffer
;; NOTE: for colors:
;; http://www.emacswiki.org/emacs/CompilationMode#toc6
;; (compile-.el, compile+.el: Extensions to compile.el)

;;; Varibles
(setq compilation-scroll-output t)
(defvar yura/compilation-truncate-lines nil
  "Truncate lines for compilation buffer.")

;;; Functions
;;;; yura/compilation-toggle-scroll-output
(defun yura/compilation-toggle-scroll-output ()
  "Toggle `compilation-scroll-output' variable."
  (interactive)
  (toggle-logical-value compilation-scroll-output)
  (message "Set compilation scroll: '%s'" compilation-scroll-output))

;;;; yura/compilation-toggle-auto-jump-to-first-error
(defun yura/compilation-toggle-auto-jump-to-first-error ()
  "Toggle variable `compilation-auto-jump-to-first-error'."
  (interactive)
  (toggle-logical-value compilation-auto-jump-to-first-error)
  (message "Auto jump to first error, set: '%s'" compilation-auto-jump-to-first-error))

;;;; yura/compilation-toggle-truncate-lines
(defun yura/compilation-toggle-truncate-lines ()
  "Toggle variable `yura/compilation-truncate-lines'.

`yura/compilation-truncate-lines' modify `truncate-lines' after restart
compilation buffer by `compilation-mode-hook'."
  (interactive)
  (toggle-logical-value yura/compilation-truncate-lines)
  (message "Truncate line (compile mode), set: '%s'" yura/compilation-truncate-lines))

;;;; compilation-finish-function
(defvar yura/compilation-finish-function nil
  "Value to override `compilation-finish-function'.")

(defun yura/compilation-toggle-finish-function (&optional show-msg)
  "Toggle `yura/compilation-finish-function'.

`yura/compilation-finish-function' modify `compilation-finish-function'
after restart compilation buffer by `compilation-mode-hook'.

Available values:
- nil
- `yura/compilation-finish-function-implicit-show-time'.

SHOW-MSG allows to display `compilation-finish-function' name."
  (interactive "P")
  (setq yura/compilation-finish-function
        (if (bound-and-true-p yura/compilation-finish-function)
            nil
          (yura/compilation-finish-function-implicit-show-time)))
  (if show-msg
      (let ((status-message "none"))
        (if (bound-and-true-p yura/compilation-finish-function)
            (setq status-message "Auto hide compilation buffer"))
        (message "compilation process finishes function: %s" status-message))))

;; https://emacs.stackexchange.com/questions/62/hide-compilation-window
(defun yura/set-compilation-finish-function-hide-buffer (&optional time)
  "Edit `yura/compilation-finish-function'.

Sets the compilation finish function by `yura/compilation-finish-function' to:
If compilation is successful(without errors),
compilation buffer will be kill in a TIME seconds.
TIME default value: \"2sec\""
  (interactive "P")
  (let ((show-time (if time
                       time
                     "2 sec")))
    (setq yura/compilation-finish-function
          `(lambda (buf str)
             (if (null (string-match ".*exited abnormally.*" str))
                 (progn
                   (run-at-time ',show-time nil 'quit-windows-on
                                (get-buffer-create "*compilation*")
                                :kill)
                   (message "No Compilation Errors!")))))))

(defvar yura/compilation-buffer-show-time "2 sec"
  "Time to show *compilation* buffer after compilation.")

(defvar yura/compilation-buffer-single-show-time nil
  "Time for a single use by command to kill *compilation* buffer.
This variable is cleared after use.")

(defun yura/compilation-finish-function-show-time ()
  "Compilation finish function to hide *compilation* buffer.

If compilation is successful(without errors),
the *compilation* buffer will be displayed for `yura/compilation-buffer-show-time' seconds."
  `(lambda (buf str)
     (if (null (string-match ".*exited abnormally.*" str))
         (progn
           (run-at-time yura/compilation-buffer-show-time
                        nil 'quit-windows-on
                        (get-buffer-create "*compilation*")
                        :kill)
           (message "No Compilation Errors!")))))

(defun yura/compilation-finish-function-implicit-show-time ()
  "Compilation finish function to hide *compilation* buffer.

If compilation is successful(without errors),
the *compilation* buffer will be displayed for
`yura/compilation-buffer-single-show-time' second if this variable not nil.
Otherwise the variable `yura/compilation-buffer-show-time'
will be used to set the *compilation* buffer display time.

Variable `yura/compilation-buffer-single-show-time' clear after usage."
  `(lambda (buf str)
     (let ((time (if yura/compilation-buffer-single-show-time
                     yura/compilation-buffer-single-show-time
                   nil))
           (default-time yura/compilation-buffer-show-time))
       (if time
           (setq yura/compilation-buffer-show-time time))
       (if (null (string-match ".*exited abnormally.*" str))
           (progn
             (run-at-time yura/compilation-buffer-show-time
                          nil 'quit-windows-on
                          (get-buffer-create "*compilation*")
                          :kill)
             (message "No Compilation Errors!")))
       (setq yura/compilation-buffer-show-time default-time
             yura/compilation-buffer-single-show-time nil))))

(setq yura/compilation-finish-function
      (yura/compilation-finish-function-implicit-show-time))

;;; Hydra
(defhydra hydra-compilation (:color red
                             :hint nil)
  "
[compilation mode]
    Toggles
--------------------------------------
  auto _j_ump to first error  (%(if compilation-auto-jump-to-first-error t nil))
  _t_runcate line             (%(if yura/compilation-truncate-lines t nil))
  _h_ide compilation buffer   (%(if (not (null yura/compilation-finish-function)) t nil))
  _s_croll compilation output (%(if compilation-scroll-output t nil))
"
  ("j" yura/compilation-toggle-auto-jump-to-first-error)
  ("t" yura/compilation-toggle-truncate-lines)
  ("h" (lambda () (interactive) (yura/compilation-toggle-finish-function t)))
  ("s" yura/compilation-toggle-scroll-output)
  ("q" nil "cancel" :color blue)
  ("C-g" nil "cancel" :color blue))
(with-eval-after-load 'compile
  (bind-key "?" #'hydra-compilation/body compilation-mode-map))

;;; Hook
(defun yura/compilation-mode-hook ()
  "Hooks for `compilation-mode'.

Replace local variables of `compilation-mode':
`truncate-lines' with `yura/compilation-truncate-lines',
`compilation-finish-function' with `yura/compilation-finish-function'."
  (setq truncate-lines yura/compilation-truncate-lines)
  (setq compilation-finish-function yura/compilation-finish-function)
  ;; Line wrapping
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook 'compilation-mode-hook #'yura/compilation-mode-hook)


(provide 'setup-compilation)
