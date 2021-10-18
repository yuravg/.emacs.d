;; Time-stamp: <2017-11-07 17:47:44 kmodi>

;; Contents:
;;
;;  Compile
;;    ansi-color
;;    *compilation* buffer
;;  Smart Compile


;;; Compile
(use-package compile
  :defer t
  :config
  (progn
;;;; ansi-color
    ;; http://stackoverflow.com/a/13408008/1219634
    (require 'ansi-color)
    (defun modi/colorize-compilation-buffer ()
      (unless (or (derived-mode-p 'grep-mode) ;Don't mess up colors in Grep/Ag results buffers
                  (derived-mode-p 'ag-mode))
        (ansi-color-apply-on-region compilation-filter-start (point))))
    (add-hook 'compilation-filter-hook #'modi/colorize-compilation-buffer)

;;;; *compilation* buffer

    ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation.html
    ;; http://stackoverflow.com/questions/1292936/line-wrapping-within-emacs-compilation-buffer
    ;; NOTE: for colors:
    ;; http://www.emacswiki.org/emacs/CompilationMode#toc6
    ;; (compile-.el, compile+.el: Extensions to compile.el)
    (setq compilation-scroll-output t)
    (defvar yura/compilation-truncate-lines nil
      "Truncate lines for compilation buffer.")

    (defun yura/compilation-toggle-scroll-output ()
      "Toggle `compilation-scroll-output' variable."
      (interactive)
      (toggle-logical-value compilation-scroll-output)
      (message "Set compilation scroll: '%s'" compilation-scroll-output))

    (defun yura/compilation-toggle-auto-jump-to-first-error ()
      "Toggle variable `compilation-auto-jump-to-first-error'."
      (interactive)
      (toggle-logical-value compilation-auto-jump-to-first-error)
      (message "Auto jump to first error, set: '%s'" compilation-auto-jump-to-first-error))

    (defun yura/compilation-toggle-truncate-lines ()
      "Toggle variable `yura/compilation-truncate-lines'.

`yura/compilation-truncate-lines' modify `truncate-lines' after restart
compilation buffer by `compilation-mode-hook'."
      (interactive)
      (toggle-logical-value yura/compilation-truncate-lines)
      (message "Truncate line (compile mode), set: '%s'" yura/compilation-truncate-lines))

    (defvar yura/compilation-finish-functions nil
      "Value to override `compilation-finish-functions'.")

    (defun yura/compilation-toggle-finish-function (&optional show-msg)
      "Toggle `yura/compilation-finish-functions'.

`yura/compilation-finish-functions' modify `compilation-finish-functions'
after restart compilation buffer by `compilation-mode-hook'.

Available values:
- nil
- `yura/compilation-finish-functions-implicit-show-time'.

SHOW-MSG allows to display `compilation-finish-functions' name."
      (interactive "P")
      (setq yura/compilation-finish-functions
            (if (bound-and-true-p yura/compilation-finish-functions)
                nil
              (yura/compilation-finish-functions-implicit-show-time)))
      (if show-msg
          (let ((status-message "none"))
            (if (bound-and-true-p yura/compilation-finish-functions)
                (setq status-message "Auto hide compilation buffer"))
            (message "compilation process finishes function: %s" status-message))))

    ;; https://emacs.stackexchange.com/questions/62/hide-compilation-window
    (defun yura/set-compilation-finish-functions-hide-buffer (&optional time)
      "Edit `yura/compilation-finish-functions'.

Sets the compilation finish function by `yura/compilation-finish-functions' to:
If compilation is successful(without errors),
compilation buffer will be kill in a TIME seconds.
TIME default value: \"2sec\""
      (interactive "P")
      (let ((show-time (if time
                           time
                         "2 sec")))
        (setq yura/compilation-finish-functions
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

    (defun yura/compilation-finish-functions-show-time ()
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

    (defun yura/compilation-finish-functions-implicit-show-time ()
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

    (setq yura/compilation-finish-functions
          (yura/compilation-finish-functions-implicit-show-time))

    (defhydra hydra-compilation (:color red
                                 :hint nil)
      "
[compilation mode]
    Toggles
--------------------------------------
  auto _j_ump to first error  (%(if compilation-auto-jump-to-first-error t nil))
  _t_runcate line             (%(if yura/compilation-truncate-lines t nil))
  _h_ide compilation buffer   (%(if (not (null yura/compilation-finish-functions)) t nil))
  _s_croll compilation output (%(if compilation-scroll-output t nil))
"
      ("j" yura/compilation-toggle-auto-jump-to-first-error)
      ("t" yura/compilation-toggle-truncate-lines)
      ("h" (lambda () (interactive) (yura/compilation-toggle-finish-function t)))
      ("s" yura/compilation-toggle-scroll-output)
      ("q" nil "cancel")
      ("C-g" nil "cancel"))
    (with-eval-after-load 'compile
      (bind-key "?" #'hydra-compilation/body compilation-mode-map))

    (defun yura/compilation-mode-hook ()
      "Hooks for `compilation-mode'.

Replace local variables of `compilation-mode':
`truncate-lines' with `yura/compilation-truncate-lines',
`compilation-finish-functions' with `yura/compilation-finish-functions'."
      (setq truncate-lines yura/compilation-truncate-lines)
      (setq compilation-finish-functions yura/compilation-finish-functions)
      ;; Line wrapping
      (set (make-local-variable 'truncate-partial-width-windows) nil))
    (add-hook 'compilation-mode-hook #'yura/compilation-mode-hook)))

;;; Smart Compile
;; https://www.emacswiki.org/emacs/SmartCompile
;; https://github.com/zenitani/elisp/blob/master/smart-compile.el
(use-package smart-compile
  :commands (modi/save-compile-execute)
  :init
  (progn
    (bind-keys
     :map modi-mode-map
     :filter (not (or (derived-mode-p 'emacs-lisp-mode)
                      (derived-mode-p 'verilog-mode)
                      (derived-mode-p 'python-mode)))
     ("<f9>" . modi/save-compile-execute)))
  :config
  (progn
    ;; Always use C99 standard for compilation
    (setcdr (assoc "\\.c\\'" smart-compile-alist) "gcc -O2 %f -lm -o %n -std=gnu99")

    (defvar modi/code-window nil
      "Variable to store the window containing the code buffer.")

    ;; http://stackoverflow.com/a/15724162/1219634
    (defun modi/do--execute (bin dir)
      "Execute BIN in eshell in DIR directory."
      (let ((default-directory dir))
        (eshell) ; Start eshell or switch to an existing eshell session
        (goto-char (point-max))
        (insert bin)
        (eshell-send-input)
        (sit-for 1) ;Let's assume the binary finishes executing in this time.
        ;; After that time, if the point is after the eshell prompt (i.e. user
        ;; input not expected), switch to the code buffer window.
        (save-excursion
          (forward-line 0)
          ;; This moves the point to the beginning of the line even if that
          ;; happens to be over the eshell prompt.
          (when (looking-at-p eshell-prompt-regexp)
            (select-window modi/code-window)))))

    (defun modi/save-compile-execute ()
      "Save, compile and execute."
      (interactive)
      (setq modi/code-window (get-buffer-window))
      (save-buffer)
      (lexical-let ((bin (smart-compile-string "./%n"))
                    ;; %n - file name without extension
                    ;; See `smart-compile-alist'.
                    finish-callback)
        (setq finish-callback
              (lambda (buf msg)
                (with-selected-window (get-buffer-window "*compilation*")
                  (bury-buffer))
                ;; If the compilation failed, bring up the buried compilation
                ;; buffer in the window to the "right". If you are already in
                ;; the right-side window, the "right" window will actually be
                ;; the left-side window as `windmove-wrap-around' is set to a
                ;; non-nil value. Else bring up eshell and execute the binary.
                ;; Save the `default-directory' for eshell before doing the
                ;; window switching.
                (let ((dir default-directory)
                      (windmove-wrap-around t))
                  (windmove-right)
                  ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2012-02/msg00133.html
                  (if (string= "finished\n" msg)
                      (progn
                        ;; Start eshell and execute the binary.
                        (modi/do--execute bin dir))
                    (switch-to-buffer "*compilation*")
                    ;; And then switch back to the code buffer window
                    (select-window modi/code-window)))
                ;; When compilation is done, execute the program and remove the
                ;; callback from `compilation-finish-functions'
                (setq compilation-finish-functions
                      (delq finish-callback compilation-finish-functions))))
        (push finish-callback compilation-finish-functions))
      (smart-compile 1))))


(provide 'setup-compile)
