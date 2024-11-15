;; Time-stamp: <2022-01-26 11:30:23 kmodi>

;; Projectile
;; https://github.com/bbatsov/projectile

(use-package projectile
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :bind (:map modi-mode-map
         ("C-c p" . hydra-projectile/body)
         ;; ("C-c f" . hydra-projectile/body) ; Bind to `hydra-source-code-warnings/body'
         ("s-f" . hydra-projectile/body))
  :commands (projectile-project-root)
  :config
  (progn
    (when (not (bound-and-true-p disable-pkg-ivy))
      (with-eval-after-load 'ivy
        (setq projectile-completion-system 'ivy)))

    ;; Do not barf when I try to do `projectile-switch-project' while in a
    ;; buffer containing a non-projectile file.
    (setq projectile-require-project-root nil)

    ;; Don't consider my home dir as a project
    (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/"))

    ;; (setq projectile-enable-caching nil)
    (setq projectile-enable-caching t) ;Enable caching, otherwise `projectile-find-file'
                                       ;is really slow for large
                                       ;projects.
    ;; Disable auto-updating of cache each time a file is opened or
    ;; deleted. The `projectile-serialize-cache' call in
    ;; `projectile-cache-current-file' (through
    ;; `projectile-find-file-hook-function') makes opening and
    ;; deleting files very slow.
    (setq projectile-auto-update-cache nil)

    (dolist (item '(".SOS" "nobackup"))
      (add-to-list 'projectile-globally-ignored-directories item))
    (dolist (item '("GTAGS" "GRTAGS" "GPATH"))
      (add-to-list 'projectile-globally-ignored-files item))
    (dolist (item projectile-globally-ignored-files)
      (add-to-list 'projectile-globally-ignored-buffers item))

    ;; Git projects should be marked as projects in top-down fashion,
    ;; so that each git submodule can be a projectile project.
    (setq projectile-project-root-files-bottom-up
          (delete ".git" projectile-project-root-files-bottom-up))
    (add-to-list 'projectile-project-root-files ".git")

    ;; Makefile and GNUMakefile can not be projectile project
    (dolist (item '("Makefile" "GNUMakefile"))
      (setq projectile-project-root-files (remove item
                                                  projectile-project-root-files)))

    (setq projectile-project-root-files-functions
          '(projectile-root-local
            projectile-root-top-down ; First look for projects in top-down order
            projectile-root-bottom-up)) ; Then in bottom-up order

    ;; Disable dynamic mode-line lighter.
    (setq projectile-mode-line-lighter "​P")
    (setq projectile-mode-line-function (lambda () projectile-mode-line-lighter))

    (defun modi/projectile-project-name (project-root)
      "Return project name after some modification if needed.

If PROJECT-ROOT begins with \"/proj/\", return the sub-directory
name inside that \"proj/\" directory as the project name remove
the directory basename."
      (cond
       ((string-match "\\`/proj/\\([^/]+\\)/" project-root)
        (match-string-no-properties 1 project-root))
       (t
        (projectile-default-project-name project-root))))
    (setq projectile-project-name-function #'modi/projectile-project-name)

    (defun modi/advice-projectile-use-ag (&rest _args)
      "Always use `ag' for getting a list of all files in the project."
      (mapconcat #'shell-quote-argument
                 (append '("ag")
                         modi/ag-arguments
                         '("-0"         ;Output null separated results
                           "-g" ""))    ;Get file names matching "" (all files)
                 " "))

    (defun modi/advice-projectile-use-rg (&rest _args)
      "Always use `rg' for getting a list of all files in the project."
      (let* ((prj-user-ignore-name (expand-file-name
                                    (concat ".ignore." user-login-name)
                                    (projectile-project-root)))
             (prj-user-ignore (when (file-exists-p prj-user-ignore-name)
                                (concat "--ignore-file " prj-user-ignore-name))))
        (mapconcat #'shell-quote-argument
                   (if prj-user-ignore
                       (append '("rg")
                               modi/rg-arguments
                               `(,prj-user-ignore)
                               '("--null" ;Output null separated results
                                 ;; Get names of all the to-be-searched files,
                                 ;; same as the "-g ''" argument in ag.
                                 "--files"))
                     (append '("rg")
                             modi/rg-arguments
                             '("--null"
                               "--files")))
                   " ")))

    ;; Use `rg' all the time if available
    (if (executable-find "rg")
        (progn
          (advice-remove 'projectile-get-ext-command #'modi/advice-projectile-use-ag)
          (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-rg))
      ;; Else use `ag' if available
      (when (executable-find "ag")
        (advice-remove 'projectile-get-ext-command #'modi/advice-projectile-use-rg)
        (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-ag)))

    ;; Make the file list creation faster by NOT calling `projectile-get-sub-projects-files'
    (defun modi/advice-projectile-no-sub-project-files ()
      "Directly call `projectile-get-ext-command'. No need to try to get a
list of sub-project files if the vcs is git."
      (projectile-files-via-ext-command (projectile-get-ext-command)))
    (advice-add 'projectile-get-repo-files :override
                #'modi/advice-projectile-no-sub-project-files)

    ;; Do not visit the current project's tags table if `ggtags-mode' is loaded.
    ;; Doing so prevents the unnecessary call to `visit-tags-table' function
    ;; and the subsequent `find-file' call for the `TAGS' file."
    (defun modi/advice-projectile-dont-visit-tags-table ()
      "Don't visit the tags table as we are using gtags/global."
      nil)
    (when (fboundp 'ggtags-mode)
      (advice-add 'projectile-visit-project-tags-table :override
                  #'modi/advice-projectile-dont-visit-tags-table))

    ;; http://emacs.stackexchange.com/a/10187/115
    (defun modi/kill-non-project-buffers (&optional kill-special)
      "Kill buffers that do not belong to a `projectile' project.

With prefix argument (`C-u'), also kill the special buffers."
      (interactive "P")
      (let ((bufs (buffer-list (selected-frame))))
        (dolist (buf bufs)
          (with-current-buffer buf
            (let ((buf-name (buffer-name buf)))
              (when (or (null (projectile-project-p))
                        (and kill-special
                             (string-match "^\*" buf-name)))
                ;; Preserve buffers with names starting with *scratch or *Messages
                (unless (string-match "^\\*\\(\\scratch\\|Messages\\)" buf-name)
                  (message "Killing buffer %s" buf-name)
                  (kill-buffer buf))))))))

    (defun modi/package-make-projectile-cache-stale (install-list delete-list)
      "Mark the .emacs.d projectile project to be updated by updating
the .projectile file for that project, when any package is installed or deleted.
The return value of this function is unused as it is added as an :after advice."
      ;; Update the .projectile file if any package is installed or deleted.
      (when (or install-list delete-list)
        (write-region "" :ignore (concat user-emacs-directory ".projectile")
                      nil :no-message-echo)))
    (advice-add 'package-menu--perform-transaction
                :after #'modi/package-make-projectile-cache-stale)
    (with-eval-after-load 'paradox
      (advice-add 'paradox--perform-package-transaction
                  :after #'modi/package-make-projectile-cache-stale))

    (defun modi/projectile-known-projects-sort ()
      "Move the now current project to the top of the `projectile-known-projects' list."
      (when-let* ((prj (projectile-project-root)))
        ;; Set `prj' to nil if that project is supposed to be ignored.
        (when (projectile-ignored-project-p prj)
          (setq prj nil))
        (when prj
          (setq prj-true (file-truename prj))
          (setq prj-abbr (abbreviate-file-name prj-true))
          ;; First remove the current project from `projectile-known-projects'.
          ;; Also make sure that duplicate instance of the project name in form of symlink
          ;; name, true name and abbreviated name, if any, are also removed.
          (setq projectile-known-projects
                (delete prj (delete prj-true (delete prj-abbr projectile-known-projects))))
          ;; Then add back only the abbreviated true name to the beginning of
          ;; `projectile-known-projects'.
          (add-to-list 'projectile-known-projects prj-abbr))))
    (add-hook 'projectile-after-switch-project-hook #'modi/projectile-known-projects-sort)

    (defun modi/projectile-find-file-literally (&optional arg)
      "Jump to a project's file literally (see `find-file-literally') using
completion.  With a prefix ARG invalidates the cache first.

Using this function over `projectile-find-file' is useful for opening files that
are slow to open because of their major mode. `find-file-literally' always opens
files in Fundamental mode."
      (interactive "P")
      (projectile-maybe-invalidate-cache arg)
      (projectile-completing-read
       "Find file literally: "
       (projectile-current-project-files)
       :action `(lambda (file)
                  (find-file-literally (expand-file-name file ,(projectile-project-root)))
                  (run-hooks 'projectile-find-file-hook))))

    (defun modi/projectile-switch-project-magit-status ()
      "Switch to other project and open Magit status there."
      (interactive)
      (let ((projectile-switch-project-action #'magit-status))
        (call-interactively #'projectile-switch-project)))

    (defun yura/projectile-switch-open-project-magit-status ()
      "Switch to a project which currently opened and open Magit status there."
      (interactive)
      (let ((projectile-switch-project-action #'magit-status))
        (call-interactively #'projectile-switch-open-project)))

    (defhydra hydra-projectile-other-window (:color teal)
      "projectile-other-window"
      ("b" projectile-switch-to-buffer-other-window "buffer")
      ("D" projectile-find-dir-other-window "dir")
      ("f" projectile-find-file-other-window "file")
      ("F" projectile-find-file-dwim-other-window "file dwim")
      ("q" nil "cancel")
      ("C-g" nil "cancel"))

    ;; 'native' projectile-indexing method takes too much time in the Windows OS.
    (if (eq system-type 'windows-nt)
        (setq projectile-indexing-method 'alien))

    ;; Compile projectile projects without prompts
    ;; NOTE: you should be very careful about opening files you do not trust,
    ;; should only use it with a trusted file
    (defun yura/projectile-compile-project ()
      "Run `projectile-compile-project' without prompt.\n
This is not a safe command, this command should only be used with trusted files."
      (interactive)
      (let ((compilation-read-command nil))
        (projectile-compile-project nil)))

    (defun yura/projectile-test-project ()
      "Run `projectile-test-project' without prompt.\n
This is not a safe command, this command should only be used with trusted files."
      (interactive)
      (let ((compilation-read-command nil))
        (projectile-test-project nil)))

    (defun yura/projectile-run-project ()
      "Run `projectile-run-project' without prompt.\n
This is not a safe command, this command should only be used with trusted files."
      (interactive)
      (let ((compilation-read-command nil))
        (projectile-run-project nil)))

    ;; see at `modi/revert-all-file-buffers'
    (defun yura/projectile-revert-all-file-buffers (reverse-modes)
      "Refresh all open projectile file buffers without confirmation.

Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed.
Prefixed with \\[universal-argument] REVERSE-MODES buffer modes will be reversed."
      (interactive "P")
      (if reverse-modes
          (message "Reverting and reinitializing ...")
        (message "Reverting ..."))
      (dolist (buf (projectile-project-buffers))
        (let ((filename (buffer-file-name buf)))
          ;; Revert only buffers containing files, which are not modified;
          ;; do not try to revert non-file buffers like *Messages*.
          (when (and filename
                     (not (buffer-modified-p buf)))
            (if (file-readable-p filename)
                ;; If the file exists and is readable, revert the buffer.
                (with-current-buffer buf
                  (revert-buffer :ignore-auto :noconfirm (unless reverse-modes :preserve-modes)))
              ;; Otherwise, kill the buffer.
              (let (kill-buffer buff)
                (message "Killed non-existing/unreadable file buffer: %s" filename))))))
      (if reverse-modes
          (message "Finished reverting buffers and reinitializing their modes.")
        (message "Finished reverting buffers.")))
    (defalias 'rbp 'yura/projectile-revert-all-file-buffers)

    ;; TODO: should usage current frame parameters for new frame
    (defun yura/projectile-switch-buffer-new-frame ()
      "Make new frame, launch `projectile-switch-to-buffer'."
      (interactive)
      (select-frame (make-frame))
      (projectile-switch-to-buffer))
    (defun yura/projectile-find-file-new-frame ()
      "Make new frame, launch `projectile-find-file'."
      (interactive)
      (select-frame (make-frame))
      (projectile-find-file))

    ;; Showing the root directory of a project is useful if there are sub-projects.
    ;; Some hydra-projectile commands are not from projectile package but I think it is
    ;; convenient to have them in one place.
    (defhydra hydra-projectile (:color teal
                                :hint  nil)
      "
Projectile %(if (fboundp 'projectile-project-root) (projectile-project-root) \"TBD\"):
^^     Find            ^^^^           Search               ^^^^     Buffers                   ^^^^   Cache/Tags             ^^^^         Run/compile              ^^   Other
^^---------------------^^^^--------------------------------^^^^-------------------------------^^^^--------------------------^^^^----------------------------------^^-------------------------------------------------
  _f_: file                  _s_/_a_: counsel rg/ag        ^^  _i_: Ibuffer                   ^^_c_: cache clear            _M-c_/_C-c_: compile/force compile    _p_/_P_: switch to an open/other project
  _F_: file dwim         _C-s_/_C-a_: rg/ag              _b_/_C-b_: switch/other window       ^^_x_: remove known project   _M-t_/_C-t_: test/force test          _g_/_G_: switch to Magit status of open/other project
  _l_: file literally  ^^        _O_: multi-occur      _M-b_/_M-f_: switch/find new frame     ^^_X_: cleanup non-existing   _M-r_/_C-r_: run/force run            ^^  _E_: edit project's .dir-locals.el
  _r_: recent file     ^^      _M-g_: git-grep             ^^  _k_: kill all                  ^^_z_: cache current          ^^^^                                  ^^  _D_: find dir
_C-f_: Git file        ^^        _w_: src-warnings         ^^_C-m_: revert all              _u_/_U_: gtags update/create    ^^^^                                  ^^  _4_: other window
^^                     ^^^^                                ^^_M-m_: revert all with modes     ^^^^                          ^^^^                                  ^^  _o_: submodule
^^                     ^^^^                                ^^^^                               ^^^^                          ^^^^                                  ^^_C-o_: submodules list
"
      ("f"   projectile-find-file)
      ("F"   projectile-find-file-dwim)
      ("l"   modi/projectile-find-file-literally)
      ("r"   projectile-recentf)
      ("C-f" counsel-git)

      ("s"   counsel-projectile-rg)
      ("a"   counsel-projectile-ag)
      ("C-s" projectile-ripgrep)
      ("C-a" projectile-ag)
      ("O"   projectile-multi-occur)
      ("M-g" counsel-git-grep)
      ("w"   yura/projectile-src-warning)

      ("i"   projectile-ibuffer)
      ("b"   projectile-switch-to-buffer)
      ("C-b" projectile-switch-to-buffer-other-window)
      ("M-b" yura/projectile-switch-buffer-new-frame)
      ("M-f" yura/projectile-find-file-new-frame)
      ("k"   projectile-kill-buffers)
      ("C-m" yura/projectile-revert-all-file-buffers)
      ("M-m" (yura/projectile-revert-all-file-buffers :reverse-modes))
      ("C-o" magit-list-submodules)

      ("c"   projectile-invalidate-cache)
      ("x"   projectile-remove-known-project)
      ("X"   projectile-cleanup-known-projects)
      ("z"   projectile-cache-current-file)
      ("U"   ggtags-create-tags)
      ("u"   ggtags-update-tags)

      ("C-c" yura/projectile-compile-project)
      ("C-t" yura/projectile-test-project)
      ("C-r" yura/projectile-run-project)

      ("M-c" projectile-compile-project)
      ("M-t" projectile-test-project)
      ("M-r" projectile-run-project)

      ("p"   projectile-switch-open-project)
      ("P"   projectile-switch-project)
      ("g"   yura/projectile-switch-open-project-magit-status)
      ("G"   modi/projectile-switch-project-magit-status)
      ("E"   projectile-edit-dir-locals)
      ("D"   projectile-find-dir)
      ("4"   hydra-projectile-other-window/body)
      ("o"   magit-submodule)

      ("q"   nil "cancel")
      ("C-g" nil "cancel"))

    (projectile-mode)))


(provide 'setup-projectile)

;; Do "touch ${PRJ_HOME}/.projectile" when updating a project. If the time stamp of
;; the ".projectile" is newer than that of the project cache then the existing
;; cache will be invalidated and recreated.
