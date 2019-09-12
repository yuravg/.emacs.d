;; Time-stamp: <2019-09-11 12:03:15 kmodi>

;; Setup for different tags

;; Contents:
;;
;;  Tags setup notes
;;    Setup Ctags(Universal Ctags or Exuberant Ctags)
;;    Setup global
;;    ggtags
;;  ctags
;;    etags-select
;;      etags-table
;;    ctags-update
;;  modi/find-tag
;;  xref, semantic/symref


;;; Tags setup notes

;;;; Setup Ctags(Universal Ctags or Exuberant Ctags)
;; 1. Overview
;;    ctags(exuberant ctags), systemverilog and emacs: https://scripter.co/ctags-systemverilog-and-emacs/
;; 2. Install Ctags
;;    2.1. Windows OS:
;;         Get ctags.exe
;;           Universal Ctags(is preferable): https://github.com/universal-ctags/ctags-win32/releases
;;           Or
;;           Exuberant Ctags: http://ctags.sourceforge.net/
;;         Copy ctags.exe to <Path>/emacs/bin/
;;    2.2. Linux OS:
;;         https://github.com/universal-ctags/ctags#how-to-build-and-install
;; 3. Configure Ctags
;;    For Universal Ctags see at https://github.com/universal-ctags/ctags#difference
;;    copy .ctags - Ctags configuration file to $HOME(for Exuberant Ctags only)
;;    https://verificationacademy.com/forums/systemverilog/ctags-systemverilog
;;
;;;; Setup global
;; 1. Download
;;    https://www.gnu.org/software/global/download.html
;;    (for Windows OS: download binary: http://adoxa.altervista.org/global/)
;; 2. Configure
;;    ./configure --prefix=/usr/local --with-exuberant-ctags=/usr/local/bin/ctags
;; Notes: https://www.reddit.com/r/emacs/comments/3pni17/ctags_etags_or_gtags/
;; 3. Setup environment
;;    for Bash:
;;      export GTAGSCONF="$HOME"/.globalrc
;;      export GTAGSLABEL="new-ctags"
;;    or use:
;;      (setenv "GTAGSCONF" (concat (getenv "HOME") "/" ".globalrc"))
;;      (setenv "GTAGSLABEL" "new-ctags")

(if (not (eq system-type 'windows-nt))
    (progn
      (setenv "GTAGSCONF" (concat (getenv "HOME") "/" ".globalrc"))
      (setenv "GTAGSLABEL" "new-ctags")))

(when (executable-find "global")
;;;; ggtags
  ;; https://github.com/leoliu/ggtags:
  ;; 1. install/configuration global to work with Emacs
  ;; 2. install pygments plugins
  (use-package ggtags
    :config
    (progn
      (setq ggtags-update-on-save nil) ;Don't try to update GTAGS on each save; makes the system sluggish for huge projects.
      (setq ggtags-highlight-tag nil)  ;Don't auto-highlight tag at point.. makes the system really sluggish!
      (setq ggtags-sort-by-nearness nil) ; Enabling nearness requires global 6.5+
      (setq ggtags-navigation-mode-lighter nil)
      (setq ggtags-mode-line-project-name nil)
      (setq ggtags-oversize-limit (* 30 1024 1024)) ; 30 MB

      (dolist (hook '(verilog-mode-hook
                      c++-mode-hook
                      c-mode-hook))
        (add-hook hook #'ggtags-mode))

      ;; Don't consider ` (back quote) as part of `tag' when looking for a
      ;; Verilog macro definition
      (defun ggtags-tag-at-point ()
        (pcase (funcall ggtags-bounds-of-tag-function)
          (`(,beg . ,end)
           (if (eq ?` (string-to-char (buffer-substring beg end)))
               ;; If `(buffer-substring beg end)' returns "`uvm_info" (for example),
               ;; discard the ` and return just "uvm_info"
               (buffer-substring (1+ beg) end)
             ;; else return the whole `(buffer-substring beg end)'
             (buffer-substring beg end)))))

      (bind-key "C-j" #'compile-goto-error ggtags-global-mode-hook)
      ;; Remove the default binding for `M-.' in `ggtags-mode-map'
      (bind-key "M-." nil ggtags-mode-map)
      ;; Remove the default binding for `M-o' in `ggtags-navigation-map'
      (bind-key "M-o" nil ggtags-navigation-map)

      (key-chord-define-global "??" #'ggtags-show-definition))))

;;; ctags
;; https://github.com/universal-ctags/ctags
;; Use Universal (earlier called Exuberant) ctags from github instead of the
;; ctags that comes with emacs.

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
(setq tags-case-fold-search nil) ; t=case-insensitive, nil=case-sensitive

;; Increase the warning threshold to be more than normal TAGS file sizes
(setq large-file-warning-threshold (* 50 1024 1024)) ; 50MB

(when (executable-find "ctags")
;;;; etags-select
  ;; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags
  (use-package etags-select
    :load-path "elisp/manually-synced/etags-select"
    :commands (modi/update-etags-table)
    :config
    (progn

;;;;; etags-table
      ;; Depending on the location of the file in buffer, the respective TAGS
      ;; file is opened on doing a tag find.
      (use-package etags-table
        :load-path "elisp/manually-synced/etags-table"
        :config
        (progn
          (setq etags-table-alist nil) ; initialize `etags-table-alist'

          ;; emacs config
          (add-to-list 'etags-table-alist
                       `(,(concat user-emacs-directory ".*")
                         ,(concat user-emacs-directory "TAGS")))

          ;; Max depth to search up for a tags file; nil means don't search
          (setq etags-table-search-up-depth 15)))

      ;; Below function comes useful when you change the project-root
      ;; symbol to a different value (when switching projects)
      (defun modi/update-etags-table ()
        "Update `etags-table-alist' based on the current project directory."
        (interactive)
        (when (and (featurep 'projectile)
                   (projectile-project-root))
          (add-to-list 'etags-table-alist
                       `(,(concat (projectile-project-root) ".*")
                         ,(concat (projectile-project-root) "TAGS"))
                       t)))

      (bind-keys
       :map etags-select-mode-map
       ("C-g" . etags-select-quit)
       ("C-j" . etags-select-goto-tag))))

;;;; ctags-update
  ;; https://github.com/jixiuf/ctags-update
  (use-package ctags-update
    :config
    (progn
      ;; Auto update
      (setq ctags-update-delay-seconds (* 30 60)) ; every 1/2 hour

      (defvar ctags-options-file (let ((file (expand-file-name ".ctags" user-home-directory)))
                                   (when (file-exists-p file)
                                     file))
        "User's Ctags options file.")

      (when ctags-options-file
        (setq ctags-update-other-options
              (list
               (concat "--options=" ctags-options-file))))

      ;; Override `ctags-update-how-to-update' so that when it called
      ;; non-interactively (via `after-save-hook), then the user is not nagged
      ;; to generate the TAGS file if it is not present. If TAGS file generation
      ;; is necessary, do M-x ctags-update.
      (defun modi/ctags-update-how-to-update (is-interactive)
        "Return the TAGS file name (maybe).

If \\[universal-argument] or \\[universal-argument] \\[universal-argument]
argument is used when calling `ctags-update' interactively, user is asked for
the location to generate the TAGS file.

If `ctags-update' is called interactively without any prefix argument, user is
asked for the TAGS file location, but only if that file is not present.

Otherwise, if `ctags-update' is called non-interactively (example, via the
`after-save-hook'), if the TAGS file is present, return that file's path; else
do nothing and return nil.

This function also prevents the user-error \"Another ctags-update process is
already running\" caused in `ctags-update' function if value returned by this
function is non-nil and the tag generation process is already running."
        (let (tags)
          (cond
           ((> (prefix-numeric-value current-prefix-arg) 1)  ;C-u or C-u C-u ,generate new tags in selected directory
            (setq tags (expand-file-name "TAGS"
                                         (read-directory-name "Generate TAGS in dir:"))))
           (is-interactive
            (setq tags (ctags-update-find-tags-file))
            (unless tags
              (setq tags (expand-file-name "TAGS"
                                           (read-directory-name "Generate TAGS in dir:")))))
           (t
            ;; If the TAGS file does not exist in this case, `tags' is set to nil.
            (setq tags (ctags-update-find-tags-file))
            (when tags
              (let ((process-already-running (get-process tags)))
                (when process-already-running
                  ;; Prevent the user-error "Another ctags-update process is
                  ;; already running" caused in `ctags-update' function if tags
                  ;; is non-nil and the tag generation process is already running.
                  (setq tags nil))))))
          tags))
      (advice-add 'ctags-update-how-to-update :override #'modi/ctags-update-how-to-update)

      (add-hook 'emacs-lisp-mode-hook #'turn-on-ctags-auto-update-mode)
      (add-hook 'verilog-mode-hook #'turn-on-ctags-auto-update-mode))))

;; Disable global/ggtags for verilog-mode for Windows OS,
;; global is slow down verilog-buffer too much.
;; I use compiled http://adoxa.altervista.org/global/,
;; and build http://adoxa.altervista.org/global/dl.php?f=source by Cygwin.
;;; modi/find-tag
(defun modi/find-tag (&optional use-ctags)
  "Use `ggtags' if available, else use `ctags' to find tags.

If USE-CTAGS is non-nil, use `ctags'.\n
If `major-mode' is `verilog-mode' use `ctags' only(for Windows OS only)."
  (interactive "P")
  ;; clean `yura/compilation-finish-function' to show ggtags/ctags messages
  (let ((is-function (if (bound-and-true-p yura/compilation-finish-function) t nil)))
    (setq yura/compilation-finish-function nil) ;clean `compilation-finish-function'
    (if (or use-ctags
            (not (featurep 'ggtags))
            (if (eq system-type 'windows-nt)
                (eq major-mode 'verilog-mode))) ;`ggtags' slow down `verilog-mode' so I disable it for this mode
        (progn
          (modi/update-etags-table)
          (etags-select-find-tag-at-point))
      (call-interactively #'ggtags-find-tag-dwim))
    (if is-function (yura/compilation-toggle-finish-function)))) ;restore `compilation-finish-function'

;;; xref, semantic/symref
;; `xref' using `semantic-symref-detect-symref-tool' and
;; `semantic-symref-calculate-rootdir' to figure out which tool is available
;; for finding definitions and references. It looks for `global', `idutils',
;; and `cscope'. If none of those are found, it defaults to `grep'.
(use-package semantic/symref
  :defer t
  :config
  (progn
    ;; The `semantic-symref-calculate-rootdir' function does not find the
    ;; "right" rootdir by default. So using `projectile-project-root' to do
    ;; that job instead.
    (with-eval-after-load 'projectile
      (defalias 'semantic-symref-calculate-rootdir 'projectile-project-root))))

(bind-keys
 :map modi-mode-map
 ;; Do not set the below binding in `emacs-lisp-mode' buffers because we do
 ;; not want to override the default "M-." binding to `xref-find-definitions'.
 :filter (not (derived-mode-p 'emacs-lisp-mode))
 ("M-." . modi/find-tag))


(provide 'setup-tags)

;; Emacs rereads the TAGS file (ctags) during every tag find operation.

;; Default `etags-select' bindings
;; |---------+------------------------------------|
;; | Binding | Description                        |
;; |---------+------------------------------------|
;; | RET     | etags-select-goto-tag              |
;; | M-RET   | etags-select-goto-tag-other-window |
;; | p       | etags-select-previous-tag          |
;; | n       | etags-select-next-tag              |
;; | q       | etags-select-quit                  |
;; | 0       | (etags-select-by-tag-number "0")   |
;; | 1       | (etags-select-by-tag-number "1")   |
;; | ..      | ..                                 |
;; | 9       | (etags-select-by-tag-number "9")   |
;; |---------+------------------------------------|

;; Default `ggtags-navigation-mode'
;; |-------------------------+-------------------------------------------------------------------------------|
;; | Binding                 | Description                                                                   |
;; |-------------------------+-------------------------------------------------------------------------------|
;; | M-n                     | Move to the next match.                                                       |
;; | M-p                     | Move to the previous match.                                                   |
;; | M-}                     | Move to next file.                                                            |
;; | M-{                     | Move to previous file.                                                        |
;; | M-=                     | Move to the file where navigation session starts.                             |
;; | M-<                     | Move to the first match.                                                      |
;; | M->                     | Move to the last match.                                                       |
;; | C-M-s or M-s s          | Use isearch to find the match.                                                |
;; | RET                     | the right match so exit navigation mode. Resumable by M-x tags-loop-continue. |
;; | M-, (M-* if Emacs < 25) | Abort and go back to the location where the search was started.               |
;; |-------------------------+-------------------------------------------------------------------------------|
