;; Time-stamp: <2019-01-29 18:49:11 kmodi>

;; Windows and buffers manipulation

;; Contents:
;;
;;  Variables
;;  Winner Mode
;;  Uniquify
;;  Recentf
;;  Find .dir-locals.el
;;  Duplicate current window
;;  Insert file name
;;  Switch/revert file/buffer coding system
;;  Windmove
;;  Reopen Killed File
;;  Current File Buffer Actions
;;  Revert buffer
;;  Scratch-and-Back
;;  Scratch buffer
;;  Minibuffer and Recursive Edit
;;  Untouchable Minibuffer Prompt
;;  Minibuffer bindings
;;  Toggle between buffers
;;  Scrolling
;;  File Permissions
;;  One Window Toggle
;;  Kill/Bury Buffer
;;  Other Window/Buffer
;;  Frame
;;    Frame setup
;;    Transpose Frame
;;    Resize frame
;;    Open at new frame
;;    Select frame
;;  Temporary buffer for read-only
;;  Help mode
;;  *Messages* Auto-tail
;;  Bindings
;;    Read-only Buffer Bindings
;;    Other Bindings

;;; Variables
;; When multiple buffers are visible (like in a frame with 2 or more windows),
;; do not display an already visible buffer when switching to next/previous
;; buffers or after killing buffers.
(setq switch-to-visible-buffer nil)

(setq recenter-positions '(0.43 0.07 0.88)) ;default: '(middle top bottom)
;; (setq recenter-positions '(0.50 0.07 0.93))
;; First C-l  -> 0.50: Put point vertically at the middle of the window
;; Second C-l -> 0.07: Put point close to the top of the window. If
;;                     (window-height) returns 70, that's roughly 4 lines.
;; Third C-l  -> 0.93: Put point close to the bottom of the window ~ 3 lines.
;; With the default values of `recenter-positions' and `scroll-margin' (0),
;; the "top" position is the first line of the window, and the "bottom"
;; position is the last line. Above settings provides a margin of 3 or 4 lines
;; for my default window size for the "top" and "bottom" iterations.

;;; Winner Mode
;; http://www.emacswiki.org/emacs/WinnerMode
;; Winner Mode is a global minor mode. When activated, it allows to “undo”
;; (and “redo”) changes in the window configuration with the key commands
;; ‘C-c left’ and ‘C-c right’
(use-package winner
  :config
  (progn
    (winner-mode 1)))

;;; Uniquify
;; The library uniquify overrides Emacs’ default mechanism for making buffer
;; names unique (using suffixes like <2>, <3> etc.) with a more sensible
;; behaviour which use parts of the file names to make the buffer names
;; distinguishable.
(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'post-forward)))

;;; Recentf
;; http://www.emacswiki.org/emacs/RecentFiles
(use-package recentf
  :defer 1
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-saved-items 2000)
    (setq recentf-max-menu-items 80)))

;;; Find .dir-locals.el
(defun find-file-dir-locals ()
  "Find the '.dir-locals.el' file in the local directory.
If the file does not exist it will be created."
  (interactive)
  (find-file "./.dir-locals.el"))

;;; Duplicate current window
(defun duplicate-current-window (arg)
  "Duplicate the current window to the next window.

Prefixed with \\[universal-argument] delete other windows."
  (interactive "P")
  (if arg
      (delete-other-windows))
  (split-window-right)
  (balance-windows))
(defalias 'db 'duplicate-current-window)

;;; Insert file name
;; https://www.emacswiki.org/emacs/InsertFileName
(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))

(defun insert-current-file-name (full-path)
  "Insert name of the current buffer after point.

Prefixed FULL-PATH with \\[universal-argument], expand the file name to its fully path."
  (interactive "P")
  (if full-path
      (insert (buffer-file-name (window-buffer (minibuffer-selected-window))))
    (insert (buffer-name))))

;; Insert file name in the 'Minibuffer'
;; Used for example after `compile' command
(bind-keys
 :map minibuffer-local-map
 ("C-c f" . (lambda (filename)
              (interactive "*fInsert file name: P")
              (insert (file-relative-name filename)))) ;insert relative path
 ("C-M-j" . (lambda (filename)
              (interactive "*fInsert file name: P")
              (insert (file-relative-name filename)))) ;insert relative path
 ;; insert current buffer name(it is recent for the 'Minibuffer')
 ("C-M-f" . (lambda () (interactive) (insert (message "%s" (other-buffer (current-buffer) 1)))))
 ("C-c M-f" . insert-file-name))                       ;insert full path and other(`insert-file-name')

;;; Switch/revert file/buffer coding system
;; http://www.emacswiki.org/emacs/EndOfLineTips
(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix :force))
(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos :force))
(defun mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-mac :force))
(defun utf-8-unix-file ()
  "Set the file coding-system of the current buffer to 'utf-8-unix."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix :force))
(defun utf-8-dos-file ()
  "Set the file coding-system of the current buffer to 'utf-8-dos."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos :force))

(defun revert-buffer-with-coding-system-no-confirm (coding-system)
  "Change `coding-system-for-read' with CODING-SYSTEM and `revert-buffer' without confirmation."
  (interactive "zCoding system for visited file (default nil): ")
  (let ((coding-system-for-read coding-system))
    (revert-buffer :noconfirm t)))

(defun revert-buffer-with-cp1251 ()
  "Revert buffer with coding: cp1251."
  (interactive)
  (revert-buffer-with-coding-system-no-confirm 'cp1251)
  (message "Buffer reverted with: cp1251"))

(defun revert-buffer-with-utf8 ()
  "Revert buffer with coding: utf-8."
  (interactive)
  (revert-buffer-with-coding-system-no-confirm 'utf-8)
  (message "Buffer reverted with: utf-8"))

(defun revert-buffer-with-utf8-dos ()
  "Revert buffer with coding: utf-8-dos."
  (interactive)
  (revert-buffer-with-coding-system-no-confirm 'utf-8-dos)
  (message "Buffer reverted with: utf-8-dos"))

(defun revert-buffer-with-utf8-unix ()
  "Revert buffer with coding: utf-8-unix."
  (interactive)
  (revert-buffer-with-coding-system-no-confirm 'utf-8-unix)
  (message "Buffer reverted with: utf-8-unix"))

(defun recode-region-from-cp1251-to-utf8 (start end)
  "Call `recode-region' from cp1251 to utf-8, between points START END."
  (interactive "r")
  (recode-region start end 'cp1251 'utf-8)
  (message "Region recode to: utf-8"))

(defun recode-region-from-utf8-to-cp1251 (start end)
  "Call `recode-region' from utf-8 to cp1251, between points START END."
  (interactive "r")
  (recode-region start end 'utf-8 'cp1251)
  (message "Region recode to: cp1251"))

(defun recode-region-from-cp1252-to-utf8 (start end)
  "Call `recode-region' from cp1252 to utf-8, between points START END.

Usage: execute this command after copying the Russian text to the utf-8 buffer."
  (interactive "r")
  (recode-region start end 'utf-8 'cp1252)
  (recode-region start end 'cp1251 'utf-8))

(defun revert-and-recode-buffer-from-cp1251-to-utf8 ()
  "Revert and recode current buffer form cp1251 to utf-8."
  (interactive)
  (revert-buffer-with-coding-system-no-confirm 'cp1251)
  (set-buffer-file-coding-system 'utf-8)
  (message "Buffer reverted and recode: cp1251 -> utf-8"))

(defun revert-and-recode-buffer-from-utf8-to-cp1251 ()
  "Revert and recode current buffer from utf-8 to cp1251."
  (interactive)
  (revert-buffer-with-coding-system-no-confirm 'utf-8)
  (set-buffer-file-coding-system 'cp1251)
  (message "Buffer reverted and recode: utf-8 -> cp1251"))

(defun revert-and-recode-buffer-from-cp866-to-utf-8 ()
  (interactive)
  "Revert and recode current buffer from cp866 to utf-8."
  (revert-buffer-with-coding-system-no-confirm 'cp866)
  (set-buffer-file-coding-system 'utf-8)
  (message "Buffer reverted and recode: cp866 -> utf-8"))

;;; Windmove
(use-package windmove
  :bind (:map modi-mode-map
         ("s-<left>" . windmove-left)
         ("s-<right>" . windmove-right)
         ("s-<up>" . windmove-up)
         ("s-<down>" . windmove-down)
         ("C-c ]" . hydra-resize-window/body)
         ("C-c [" . hydra-resize-window/body))
  :config
  (progn
    (setq windmove-wrap-around t)       ;default = nil

    ;; Move window splitters / Resize windows
    ;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
    (defun modi/move-splitter (direction delta)
      "Move horizontal/vertical splitter in DIRECTION by DELTA columns."
      (let* ((windmove-wrap-around nil)
             (win-left (windmove-find-other-window 'left))
             (win-right (windmove-find-other-window 'right))
             (win-up (windmove-find-other-window 'up))
             (win-down (let ((win (windmove-find-other-window 'down)))
                         ;; Do not consider minibuffer as a valid window when
                         ;; figuring out if the current window is the bottom-most
                         ;; window.
                         (if (window-minibuffer-p win) nil win))))
        (cond
         ((and (or (eq direction 'left)
                   (eq direction 'right))
               (null win-left)
               (null win-right))
          (user-error "Cannot resize width, window occupies full frame width"))
         ((and (or (eq direction 'up)
                   (eq direction 'down))
               (null win-up)
               (null win-down))
          (user-error "Cannot resize height, window occupies full frame height"))
         (t
          (cl-case direction
            (left (cond
                   (win-right (shrink-window-horizontally delta))
                   (win-left (enlarge-window-horizontally delta))))
            (right (cond
                    (win-left (shrink-window-horizontally delta))
                    (win-right (enlarge-window-horizontally delta))))
            (up (cond
                 (win-down (shrink-window delta))
                 (win-up (enlarge-window delta))))
            (down (cond
                   (win-up (shrink-window delta))
                   (win-down (enlarge-window delta)))))))))

    (defun modi/move-splitter-left (delta)
      "Move window splitter left."
      (interactive "p")
      (modi/move-splitter 'left delta))

    (defun modi/move-splitter-right (delta)
      "Move window splitter right."
      (interactive "p")
      (modi/move-splitter 'right delta))

    (defun modi/move-splitter-up (delta)
      "Move window splitter up."
      (interactive "p")
      (modi/move-splitter 'up delta))

    (defun modi/move-splitter-down (delta)
      "Move window splitter down."
      (interactive "p")
      (modi/move-splitter 'down delta))

    (defhydra hydra-resize-window (:color red)
      "resize window"
      ("<left>" modi/move-splitter-left "↤")
      ("<right>" modi/move-splitter-right "↦")
      ("<up>" modi/move-splitter-up "↥")
      ("<down>" modi/move-splitter-down "↧")
      ("[" modi/move-splitter-left nil)
      ("]" modi/move-splitter-right nil)
      ("{" modi/move-splitter-up nil) ;Shift + [
      ("}" modi/move-splitter-down nil) ;Shift + ]
      ("=" balance-windows "Balance")
      ("+" balance-windows nil)
      ("q" nil "cancel")
      ("C-g" nil "cancel")
      ("<return>" nil "cancel"))))

;;; Reopen Killed File
;; http://emacs.stackexchange.com/a/3334/115
(defvar killed-file-list nil
  "List of recently killed files.")

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (if killed-file-list
      (find-file (pop killed-file-list))
    (message "No recently killed file found to reopen.")))

;;; Current File Buffer Actions
;; Delete current buffer file
(defun modi/delete-current-buffer-file ()
  "Deletes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and filename
               (file-exists-p filename)
               (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      (message "File `%s' successfully deleted." filename))
    (kill-buffer (current-buffer))))

;; Rename current buffer file
;; http://www.whattheemacsd.com/
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer `%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named `%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File `%s' successfully renamed to `%s'."
                   name (file-name-nondirectory new-name)))))))

;; Display the file path of the file in current buffer and also copy it to
;; the kill-ring
;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun modi/copy-buffer-file-name (option &optional quiet)
  "Show the full path to the current file in the minibuffer and also copy it.

If the full file path has a sub-string \"xyz/\" where xyz is the
user name, replace that with \"${USER}/\".

If OPTION is \\='(4), copy only the file name (not the full path).
If OPTION is \\='(16), copy the full path without the environment
variable replacement.

If QUIET is non-nil, do not print the \"Copied file name ..\" message.

Return the copied file name."
  (interactive "P")
  (let* ((file-name-full (buffer-file-name))
         (file-name (when file-name-full
                      (cl-case (car option)
                        (4 (file-name-nondirectory file-name-full)) ;C-u
                        (16 file-name-full)                         ;C-u C-u
                        (t ;If $USER==xyz, replace xyz/ with ${USER}/ in file name
                         (replace-regexp-in-string ;No prefix
                          (concat user-login-name "/") "${USER}/" file-name-full))))))
    (if file-name
        (progn
          (kill-new file-name)
          (unless quiet
            (message "Copied file name `%s'" file-name))
          file-name)                    ;Return value
      (error "Buffer not visiting a file")
      nil)))

;;; Revert buffer
(defun modi/revert-all-file-buffers (reverse-modes)
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed.
Prefixed with \\[universal-argument] REVERSE-MODES buffer modes will be reversed."
  (interactive "P")
  (if reverse-modes
      (message "Reverting and reinitializing ...")
    (message "Reverting ..."))
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; (message "buf:%s  filename:%s  modified:%s  filereadable:%s"
      ;;          buf filename
      ;;          (buffer-modified-p buf) (file-readable-p (format "%s" filename)))

      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm (unless reverse-modes :preserve-modes)))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ;No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (if reverse-modes
      (message "Finished reverting buffers and reinitializing their modes.")
    (message "Finished reverting buffers.")))
(defalias 'rba 'modi/revert-all-file-buffers)

(defun modi/revert-noconfirm-help-buffers (&rest args)
  "Don't confirm when reverting *Help* buffers."
  (list (car args) :noconfirm))
(advice-add 'help-mode-revert-buffer :filter-args #'modi/revert-noconfirm-help-buffers)

(defun yura/revert-buffer-no-confirm (reverse-modes)
  "Refresh current buffer without confirmation, buffer modes will be preserve.

Prefixed with \\[universal-argument] REVERSE-MODES buffer modes will be reversed."
  (interactive "P")
  (revert-buffer :ignore-auto :noconfirm (unless reverse-modes :preserve-modes))
  (if reverse-modes
      (message "Finished reverting buffer and reinitialize modes.")
    (message "Finished reverting buffer.")))
(defalias 'rb 'yura/revert-buffer-no-confirm)

;;; Scratch-and-Back
;; http://emacs.stackexchange.com/a/81/115
(defun modi/switch-to-scratch-and-back (&optional arg)
  "Toggle between *scratch-MODE* buffer and the current buffer.
If a scratch buffer does not exist, create it with the major mode set to that
of the buffer from where this function is called.

        COMMAND -> Open/switch to a scratch buffer in the current buffer's major mode
    C-0 COMMAND -> Open/switch to a scratch buffer in `fundamental-mode'
    C-u COMMAND -> Open/switch to a scratch buffer in `org-mode'
C-u C-u COMMAND -> Open/switch to a scratch buffer in `emacs-elisp-mode'

Even if the current major mode is a read-only mode (derived from `special-mode'
or `dired-mode'), we would want to be able to write in the scratch buffer. So
the scratch major mode is set to `org-mode' for such cases.

Return the scratch buffer opened."
  (interactive "p")
  (if (and (or (null arg)               ;No prefix
               (= arg 1))
           (string-match-p "\\*scratch" (buffer-name)))
      (switch-to-buffer (other-buffer))
    (let* ((mode-str (cl-case arg
                       (0 "fundamental-mode") ;C-0
                       (4 "org-mode")         ;C-u
                       (16 "emacs-lisp-mode") ;C-u C-u
                       ;; If the major mode turns out to be a `special-mode'
                       ;; derived mode, a read-only mode like `help-mode', open
                       ;; an `org-mode' scratch buffer instead.
                       (t (if (or (derived-mode-p 'special-mode) ;No prefix
                                  (derived-mode-p 'dired-mode))
                              "org-mode"
                            (format "%s" major-mode)))))
           (buf (get-buffer-create (concat "*scratch-" mode-str "*"))))
      (switch-to-buffer buf)
      (funcall (intern mode-str))    ;http://stackoverflow.com/a/7539787/1219634
      buf)))

;;; Scratch buffer
(setq initial-major-mode 'emacs-lisp-mode) ;; Elisp as default

;;; Minibuffer and Recursive Edit
;; Quit the minibuffer automatically when focus moves away from it (which could
;; have happened by actions like clicking some other buffer using the mouse or
;; by hitting `C-x o'). This is to avoid the irritating occasions where repeated
;; `C-g' pressing doesn't kill the minibuffer prompt as emacs has entered a
;; recursive edit session.
;; http://stackoverflow.com/a/3024055/1219634
;; The right way to exit a recursive edit session is by hitting `C-]', which is
;; bound to `abort-recursive-edit' by default.
(defun abort-recursive-edit-in-minibuffer ()
  "Disable recursive edit in minibuffer if `disable-recursive-edit-in-minibuffer'
is set to a non-nil value."
  (when (and (bound-and-true-p disable-recursive-edit-in-minibuffer)
             (active-minibuffer-window)
             (>= (recursion-depth) 1))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook #'abort-recursive-edit-in-minibuffer)

;; http://oremacs.com/2016/06/06/counsel-set-variable/
(when (not (bound-and-true-p disable-recursive-edit-in-minibuffer))
  ;; Allow to read from minibuffer while in minibuffer.
  (setq enable-recursive-minibuffers t)
  ;; Show the minibuffer depth (when larger than 1)
  (minibuffer-depth-indicate-mode 1))

;;; Untouchable Minibuffer Prompt
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21874
;; Do not allow the cursor to go over or select the minibuffer prompt.
;; A good example is that we wouldn't want to ever edit/select the "Find file:"
;; prompt we see in the minibuffer when we do `find-file'.
(>=e "25.0"
    (let (;; (get ..)                   -> ((quote (read-only t face minibuffer-prompt)))
          ;; (car (get ..))             -> (quote (read-only t face minibuffer-prompt))
          ;; (eval (car (get ..)))      -> (read-only t face minibuffer-prompt)
          ;; http://thread.gmane.org/gmane.emacs.devel/202463/focus=202496
          (default (eval (car (get 'minibuffer-prompt-properties 'standard-value))))
          (dont-touch-prompt-prop '(cursor-intangible t)))
      (setq minibuffer-prompt-properties (append default dont-touch-prompt-prop))
      ;; Note: If the above `minibuffer-prompt-properties' is set using the
      ;; Customize interface, `cursor-intangible-mode' would be automatically
      ;; added to `minibuffer-setup-hook' because of the presence of
      ;; `cursor-intangible' property in `minibuffer-prompt-properties'.
      ;; (see cus-start.el).
      (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)))

;;; Minibuffer bindings
(bind-keys
 :map minibuffer-local-map
 ("C-h" . backward-delete-char)
 ("C-w" . backward-kill-word))

;;; Toggle between buffers
;; http://www.emacswiki.org/emacs/SwitchingBuffers
(defun toggle-between-buffers ()
  "Toggle between 2 buffers"
  (interactive)
  (switch-to-buffer (other-buffer)))
;; (other-buffer &optional BUFFER VISIBLE-OK FRAME)
;; - Return most recently selected buffer other than BUFFER. Ignore the argument
;;   BUFFER unless it denotes a live buffer.
;; - If VISIBLE-OK==1, a buffer is returned even when it is visible in a split
;;   window.Buffers not visible in windows are preferred to visible buffers,
;;   unless optional second argument VISIBLE-OK is non-nil.
;; - If the optional third argument FRAME is non-nil, use that frame's buffer
;;   list instead of the selected frame's buffer list.

;;; Scrolling
;; Keep point at its screen position if the scroll command moved it vertically
;; out of the window, e.g. when scrolling by full screens using C-v.
(setq scroll-preserve-screen-position t)

;; Scroll without moving the point/cursor
(defun modi/scroll-up (ln)
  "Scroll up by LN lines without moving the point.
If LN is nil, defaults to 1 line."
  (interactive "p")
  (scroll-up ln))

(defun modi/scroll-down (ln)
  "Scroll down by LN lines without moving the point.
If LN is nil, defaults to 1 line."
  (interactive "p")
  (scroll-down ln))

;; https://github.com/politza/pdf-tools/issues/227#issuecomment-242100968
(defun modi/scroll-other-window (ln)
  "Scroll the buffer in other window.

This command supports pdf file buffers too (`pdf-view-mode').

If LN is positive, scroll the buffer up.
If LN is negative, scroll the buffer down."
  (interactive "p")
  (let ((other-win (other-window-for-scrolling)))
    (if (and (fboundp #'pdf-util-pdf-window-p)
             (pdf-util-pdf-window-p other-win))
        (with-current-buffer (window-buffer other-win)
          (with-selected-window other-win
            (if (>= ln 1)
                (pdf-view-next-line-or-next-page ln)
              (pdf-view-previous-line-or-previous-page (- ln))))
          (set-window-point other-win (point)))
      (if (modi/mouse-scroll-p last-input-event) ;defined in `setup-mouse.el'
          ;; If using mouse to scroll the other window, respect the scroll
          ;; amount set in `mouse-wheel-scroll-amount'.
          (let* ((mouse-ln-1 (car mouse-wheel-scroll-amount))
                 (mouse-ln (if (natnump ln)
                               mouse-ln-1 ;scroll up
                             (- mouse-ln-1)))) ;scroll down
            (scroll-other-window mouse-ln))
        (scroll-other-window ln)))))

(defalias 'modi/scroll-other-window-up 'modi/scroll-other-window)

(defun modi/scroll-other-window-down (ln)
  "Scroll other window down by LN lines without moving the point.
If LN is nil, defaults to 1 line."
  (interactive "p")
  (modi/scroll-other-window (- ln)))

;; Below bindings are made in global map and not in my minor mode as I want
;; to allow other modes to override these.
(bind-keys
 ("<C-M-up>" . modi/scroll-down)
 ("<C-M-down>" . modi/scroll-up)
 ("<C-M-left>" . modi/scroll-other-window-down)
 ("<C-M-right>" . modi/scroll-other-window-up))

;;; File Permissions
(defun modi/set-file-permissions (perm)
  "Change permissions of the file in current buffer.
Example: M-644 M-x modi/set-file-permissions."
  (interactive "p")
  (when (<= perm 1)
    (setq perm 644))
  (let ((cmd (concat "chmod "
                     (format "%s " perm)
                     (buffer-file-name))))
    (message "%s" cmd)
    (shell-command cmd "*Shell Temp*")
    (kill-buffer "*Shell Temp*")))

;;; One Window Toggle
(defvar modi/toggle-one-window--buffer-name nil
  "Variable to store the name of the buffer for which the `modi/toggle-one-window'
function is called.")
(defvar modi/toggle-one-window--window-configuration nil
  "Variable to store the window configuration before `modi/toggle-one-window'
function was called.")
(defun modi/toggle-one-window (&optional force-one-window)
  "Toggles the frame state between deleting all windows other than
the current window and the windows state prior to that."
  (interactive "P")
  (if (or (null (one-window-p))
          force-one-window)
      (progn
        (setq modi/toggle-one-window--buffer-name (buffer-name))
        (setq modi/toggle-one-window--window-configuration (current-window-configuration))
        (delete-other-windows))
    (progn
      (when modi/toggle-one-window--buffer-name
        (set-window-configuration modi/toggle-one-window--window-configuration)
        (switch-to-buffer modi/toggle-one-window--buffer-name)))))

;;; Kill/Bury Buffer

;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2e4f4c9d48c563ff8bec102b66da0225587786c6
(>=e "26.0"
    nil  ;The `kill-current-buffer' command will be defined in core in emacs 26+
  (defun kill-current-buffer ()
    "Kill the current buffer.
When called in the minibuffer, get out of the minibuffer
using `abort-recursive-edit'.

This is like `kill-this-buffer', but it doesn't have to be invoked
via the menu bar, and pays no attention to the menu-bar's frame."
    (interactive)
    (let ((frame (selected-frame)))
      (if (and (frame-live-p frame)
               (not (window-minibuffer-p (frame-selected-window frame))))
          (kill-buffer (current-buffer))
        (abort-recursive-edit)))))

(defun modi/kill-buffer-dwim (kill-next-error-buffer)
  "Kill the current buffer.
When called in the minibuffer, get out of the minibuffer
using `abort-recursive-edit'.

If KILL-NEXT-ERROR-BUFFER is non-nil, kill the `next-error' buffer.
Examples of such buffers: *gtags-global*, *ag*, *Occur*, *Diff*."
  (interactive "P")
  (if kill-next-error-buffer
      (kill-buffer (next-error-find-buffer :avoid-current))
    (kill-current-buffer)))

(defun modi/quit-and-kill-window ()
  "Quit window and kill instead of burying the buffer in it."
  (interactive)
  (quit-window :kill))

;;; Other Window/Buffer
;; I prefer to split the (diff) window horizontally: if `split-height-threshold' is nil,
;; `split-window-sensibly' is not allowed to split a window vertically
(setq split-height-threshold nil)

;; http://emacs.stackexchange.com/q/22226/115
(defhydra hydra-other-window-buffer
  (global-map "C-x"
              :color red)
  "other window/buffer"
  ("<right>" other-window "→win")
  ("<left>" (lambda () (interactive) (other-window -1)) "win←")
  ("<C-right>" next-buffer "→buf")
  ("<C-left>" previous-buffer "buf←"))

;; http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun switch-to-recent-buffer ()
  "Switch to a previously opened buffer.

Repeated calls switch between the last two open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun yura/other-window-bufer ()
  "Select another window or switch between buffers.

If there is only one window in frame, then switch to the last open buffer.
Otherwise switch to another window in cyclic ordering of windows."
  (interactive)
  (if (= (length (window-list)) 1)
      (switch-to-buffer (other-buffer (current-buffer) 1))
    (other-window 1)))

(defhydra hydra-scroll-other-window (:color amaranth :hint nil)
  "
Scroll other window, up/down lines: _p_/_n_: one  _C-p_/_C-n_: ten  _M-p_/_M-p_: fifty  _C-M-p_/_C-M-n_: screen  _b_/_e_: begin/end   "
  ("p"     (scroll-other-window  -1))
  ("n"     (scroll-other-window   1))
  ("C-p"   (scroll-other-window -10))
  ("C-n"   (scroll-other-window  10))
  ("M-p"   (scroll-other-window -50))
  ("M-n"   (scroll-other-window  50))
  ("C-M-p" (scroll-other-window  '-))
  ("C-M-n" (scroll-other-window    ))
  ("b"     beginning-of-buffer-other-window)
  ("e"     end-of-buffer-other-window)
  ("q"     nil "cancel")
  ("C-g"   nil "cancel"))
(bind-keys :map modi-mode-map ("C-c C-'" . hydra-scroll-other-window/body))

;;; Frame
;;;; Frame setup
(defun modi/frame-setup-1 ()
  "Set the frame to fill the center screen."
  (interactive)
  (let ((frame-resize-pixelwise t))   ;Do not round frame sizes to character h/w
    (set-frame-position nil 2560 0)   ;Pixels x y from upper left
    (set-frame-size nil 2540 1380 :pixelwise))) ;Width, height

(defun modi/frame-width-2x (double)
  "Set the frame text width to half the current width.
If DOUBLE is non-nil, the frame text width is doubled. "
  (interactive "P")
  (let ((frame-resize-pixelwise t)    ;Do not round frame sizes to character h/w
        (factor (if double 2 0.5)))
    (set-frame-size nil
                    (round (* factor (frame-text-width))) (frame-text-height)
                    :pixelwise)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;; Transpose Frame
;; http://www.emacswiki.org/emacs/transpose-frame.el
(use-package transpose-frame
  :bind (:map modi-mode-map
         ("C-c o" . rotate-frame)
         ("C-c C-\\" . transpose-frame))) ;Toggles between horz/vert splits

;;;; Resize frame
(defun yura/resize-frame ()
  "Resize the current frame to the next size.

Size list: default, maximized, full both, full height, full width."
  (interactive)
  (let* ((modes '(nil maximized fullboth fullheight fullwidth))
         (cm (cdr (assoc 'fullscreen (frame-parameters))))
         (next (cadr (member cm modes))))
    (modify-frame-parameters
     (selected-frame)
     (list (cons 'fullscreen next)))))

;;;; Open at new frame
(defun yura/buffer-to-new-frame (arg)
  "Open current buffer at new frame.
Perform an action based on ARG described below.

Prefixed with \\[universal-argument] open at new maximized frame.
Prefixed with \\[universal-argument] \\[universal-argument] open at new full-screen frame."
  (interactive "p")
  (let ((frame-value
         (cl-case arg
           (0 nil)
           (4 'maximized)
           (16 'fullboth))))
    (make-frame (list (cons 'fullscreen frame-value)))))

;; Define commands name to show them names at mini-buffer after binding.
;; For example after bind `buffer-to-maximized-frame' to 'C-x 5 n'.
(defun buffer-to-maximized-frame ()
  (interactive)
  (yura/buffer-to-new-frame 4))
(defun buffer-to-fullscreen-frame ()
  (interactive)
  (yura/buffer-to-new-frame 16))
(defun yura/counsel-recentf-new-frame ()
  "Make new frame and launch `counsel-recentf'."
  (interactive)
  (select-frame (make-frame))
  (counsel-recentf))

;;;; Select frame
(defun other-frame-next (arg)
  "Select next visible frame.

Without ARG will be selected next frame in the opposite order(from left to right).
Prefixed with \\[universal-argument] will be selected next frame in the forward order."
  (interactive "P")
  (if arg
      (other-frame 1)
    (other-frame -1)))

;;; Temporary buffer for read-only
;; new temporary buffer, which is read-only
;; https://github.com/jackkamm/undo-propose-el
(use-package undo-propose
  :defer 10
  :commands undo-propose
  :bind
  (:map undo-propose-mode-map
   ("?" . hydra-undo-propose/body))
  :init
  (progn
    (defalias 'tb 'undo-propose)  ;Temp Buffer
    (defalias 'up 'undo-propose)) ;Undo-Propose
  :config
  (progn
    ;; Open undo-propose buffer in a new window
    (setq undo-propose-pop-to-buffer t)
    (defhydra hydra-undo-propose (:color teal :hint nil)
      "
Undo-propose (map prefix: C-c):
  _C-c_: commit
  _C-s_: squash commit
  _C-d_: diff
  _C-k_: cancel
"
      ("C-c" undo-propose-commit)
      ("C-s" undo-propose-squash-commit)
      ("C-d" undo-propose-diff)
      ("C-k" undo-propose-cancel)
      ("q"   nil "cancel")
      ("C-g" nil "cancel"))))

;;; Help mode
(bind-keys
 :map help-mode-map
 ("b" . help-go-back)
 ("f" . help-go-forward)
 ("C-j" . push-button))

;;; *Messages* Auto-tail
;; Improved upon http://stackoverflow.com/a/4685005/1219634
(defun modi/messages-auto-tail (&rest _)
  "Make *Messages* buffer auto-scroll to the end after each message."
  (let* ((buf-name "*Messages*")
         ;; Create *Messages* buffer if it does not exist
         (buf (get-buffer-create buf-name)))
    ;; Activate this advice only if the point is _not_ in the *Messages* buffer
    ;; to begin with. This condition is required; otherwise you will not be
    ;; able to use `isearch' and other stuff within the *Messages* buffer as
    ;; the point will keep moving to the end of buffer :P
    (when (not (string= buf-name (buffer-name)))
      ;; Go to the end of buffer in all *Messages* buffer windows that are
      ;; *live* (`get-buffer-window-list' returns a list of only live windows).
      (dolist (win (get-buffer-window-list buf-name nil :all-frames))
        (with-selected-window win
          (goto-char (point-max))))
      ;; Go to the end of the *Messages* buffer even if it is not in one of
      ;; the live windows.
      (with-current-buffer buf
        (goto-char (point-max))))))
(advice-add 'message :after #'modi/messages-auto-tail)
;; (advice-remove 'message #'modi/messages-auto-tail)

;;; Bindings
;;;; Read-only Buffer Bindings
;; Update bindings in few read-only modes
;; http://stackoverflow.com/a/27091776/1219634
;; Cannot set below to `'(map1 map2)'; it has to be `(list map1 map2)'.
(defconst modi/read-only-mode-maps (list special-mode-map
                                         tabulated-list-mode-map)
  "List of read-only mode maps in which few key bindings need to be updated.")
(dolist (map modi/read-only-mode-maps)
  (define-key map (kbd "y") #'bury-buffer)                ;Only bury
  (define-key map (kbd "k") #'modi/kill-buffer-dwim)      ;Only kill
  (define-key map (kbd "z") #'quit-window)                ;Quit + bury
  (define-key map (kbd "q") #'modi/quit-and-kill-window)  ;Quit + kill
  (define-key map (kbd "K") #'modi/keep-lines-force)) ;Useful in eww, package manager

;;;; Other Bindings
(bind-keys
 :map modi-mode-map
 ("C-c C-m" . execute-extended-command) ;using 'C-c C-m' is more handy than 'M-x'(for me)
 ("C-'" . yura/other-window-bufer)
 ("C-x M-f" . find-name-dired)
 ("C-M-'" . switch-to-recent-buffer) ;Default binding to `drag-stuff-right'
 ("<f12>" . yura/resize-frame)
 ("C-x M-o" . other-frame)
 ("C-c C-o" . other-frame-next) ;Default: `org-open-at-point', rebind it in setup-org.el
 ("C-x 1" . modi/toggle-one-window) ;Default binding to `delete-other-windows'
 ("C-x <delete>" . modi/delete-current-buffer-file) ;Default binding to `backward-kill-sentence'
 ("C-x C-p" . modi/copy-buffer-file-name) ;Default binding to `mark-page'
 ("C-x C-r" . rename-current-buffer-file)
 ("C-S-t" . reopen-killed-file) ;Mimick "reopen last closed tab" in browsers
 ("C-c 6" . reopen-killed-file) ;Alternative to C-S-t for terminal mode
 ("C-(" . toggle-between-buffers)
 ("C-c (" . toggle-between-buffers)     ;Alternative to C-( for terminal mode
 ("C-)" . modi/kill-buffer-dwim)
 ("C-S-k" . modi/kill-buffer-dwim) ;Alternative to C-) (or C-S-0) as Windows steals that binding!
 ("C-c )" . modi/kill-buffer-dwim) ;Alternative to C-) for terminal mode
 ("C-c 0" . modi/kill-buffer-dwim))     ;Alternative to C-) for terminal mode

;; Below bindings are made in global map as I want them to work even when my
;; minor mode is disabled
(bind-keys
 ("<f5>" . revert-buffer)
 ("C-c 5" . revert-buffer)              ;Alternative to f5 for terminal mode
 ("<S-f5>" . modi/revert-all-file-buffers)
 ("C-x 5 n" . buffer-to-maximized-frame)
 ("C-x 5 C-n" . buffer-to-fullscreen-frame)
 ("C-x 5 o" . yura/counsel-recentf-new-frame) ;default binding `other-frame'
 ("C-x 5 M-o" . yura/counsel-recentf-new-frame)) ; same as: M-o - binding for `counsel-recentf'

(defalias 'arm 'auto-revert-mode)
(defalias 'ff 'toggle-frame-fullscreen)
(defalias 'ro 'read-only-mode)

(bind-to-modi-map "b" #'modi/switch-to-scratch-and-back)
(bind-to-modi-map "f" #'modi/frame-setup-1)
(bind-to-modi-map "F" #'modi/frame-width-2x)
(bind-to-modi-map "y" #'bury-buffer)

(key-chord-define-global "XX" #'modi/kill-buffer-dwim)
(key-chord-define-global "ZZ" #'toggle-between-buffers)


(provide 'setup-windows-buffers)

;; TIPS

;; (1) `C-l'
;; C-l calls the `recenter-top-bottom' command. But typing C-l twice in a row
;; scrolls the window so that point is on the topmost screen line.  Typing a
;; third C-l scrolls the window so that point is on the bottom-most screen
;; line. Each successive C-l cycles through these three positions.
;;
;; (2) Minibuffer
;; Insert buffer name in minibufer for compile command (see at: setup-windows-buffers.el):
;; `C-c f', `C-M-j' - select buffer name and insert its relative path into 'Minibuffer'
;; `C-M-f' - insert current buffer name into 'Minibuffer'
;; `C-c M-f' - select buffer name and insert its full path into 'Minibuffer'
