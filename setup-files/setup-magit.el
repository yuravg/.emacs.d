;; Time-stamp: <2021-10-22 13:28:59 kmodi>

;; Contents:
;;
;;  Magit
;;  Magit-log
;;  Git-timemachine
;;  Git-modes
;;  Emojify

;;; Magit
;; https://github.com/magit/magit
(use-package magit
  :bind (:map modi-mode-map
         ("C-x g" . magit-status)
         ("C-c g". hydra-magit/body)
         ("C-c C-g" . magit-run))
  :init
  ;; Mark the `yura/magit-diff-rh-auto-set-enable' variable as safe so that it can be
  ;; set in `.dir-locals.el' files or set in Local Variables.
  ;; Magit package loaded after use-package/commands,
  ;; so do not put a safe local variable using use-package/config
  (put 'yura/magit-diff-rh-auto-set-enable 'safe-local-variable #'booleanp)
  :commands (magit-status magit-log-all-branches)
  :config
  (progn
    ;; Speed up Magit a bit
    ;; https://magit.vc/manual/magit/Performance.html
    (setq magit-refresh-status-buffer nil)

    ;; https://github.com/hlissner/doom-emacs/blob/1456108d5bef89195b2b63f962e0ae9d24f4c652/modules/tools/magit/config.el#L23
    (setq magit-diff-refine-hunk t) ;Show granular diffs in selected hunk.
    ;; Don't autosave repo buffers. This is too magical, and saving can
    ;; trigger a bunch of unwanted side-effects, like save hooks and
    ;; formatters. Trust the user to know what they're doing.
    (setq magit-save-repository-buffers nil)
    ;; Don't display parent/related refs in commit buffers; they are rarely
    ;; helpful and only add to runtime costs.
    (setq magit-revision-insert-related-refs nil)

    ;; ;; https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

    ;; Mon Sep 27 13:39:10 EDT 2021 - kmodi
    ;; Later I realized that the major contributor to the slowness of magit-status was that
    ;; the submodules were being recursed in the `git status' and `git diff` commands (which are
    ;; called by `magit-insert-untracked-files' and `magit-insert-unstaged-changes' functions
    ;; respectively. Doing this once in the git repo with a lot of submodules fixed the magit-status
    ;; sluggishness:
    ;;
    ;;     git config diff.ignoreSubmodules all

    ;; The function used to actually bury or kill the current buffer
    ;; `magit-bury-buffer-function' bind to 'q' for magit-status, magit-log buffers.
    (setq magit-bury-buffer-function #'magit-restore-window-configuration)

    (bind-keys
     :map magit-status-mode-map
     ("C-j" . magit-diff-show-or-scroll-up)
     ("C-o" . magit-list-submodules)
     ("M-C-o" . magit-list-submodules)
     :map magit-log-mode-map
     ("C-j" . magit-diff-show-or-scroll-up)
     ("C-o" . magit-list-submodules)
     ("M-C-o" . magit-list-submodules)
     ("M-r" . magit-diff-range)
     :map magit-diff-mode-map
     ("C-j" . magit-diff-show-or-scroll-up)
     :map magit-cherry-mode-map
     ("C-j" . magit-diff-show-or-scroll-up)
     :map magit-submodule-list-mode-map
     ("C-j" . magit-repolist-status)
     ("o"   . magit-submodule))

    (setq magit-diff-refine-hunk 'all)
    ;; it takes a long time to open a large buffer with show all difference
    ;; when `magit-diff-refine-hunk' has value 'all
    (defvar yura/magit-diff-rh-max-chars 10000
      "Maximum buffer size in chars to usage `magit-diff-refine-hunk' with value 'all.")

    (defun yura/magit-diff-rh-auto-set ()
      "Toggle `magit-diff-refine-hunk' depending on buffer size.

If buffer size in char more then `yura/magit-diff-rh-max-chars'
toggle `magit-diff-refine-hunk' to nil.
Otherwise toggle to 'all.

Should usage with:
`magit-status-mode-hook', `magit-revision-mode-hook', `magit-diff-mode-hook', `magit-log-mode', etc."
      (progn
        (setq magit-diff-refine-hunk nil)
        ;; Command `magit-refresh-buffer' may and with error:
        ;; apply: Wrong type argument: number-or-marker-p, magit-log-margin-width
        ;; perhaps the reason is errors in Git repository
        (condition-case nil
            (magit-refresh-buffer)
          (error nil))
        (if (< (buffer-size) yura/magit-diff-rh-max-chars)
            (setq magit-diff-refine-hunk 'all)
          (setq magit-diff-refine-hunk nil))))

    (defvar yura/magit-diff-rh-auto-set-enable nil
      "Enable usage `yura/magit-diff-rh-auto-set'.")

    (defun yura/magit-diff-refine-hunk-auto-setting-toggle ()
      "Toggle usage `yura/magit-diff-rh-auto-set'."
      (interactive)
      (if yura/magit-diff-rh-auto-set-enable
          (progn (remove-hook 'magit-status-mode-hook  #'yura/magit-diff-rh-auto-set)
                 (remove-hook 'magit-revision-mode-hook #'yura/magit-diff-rh-auto-set)
                 (remove-hook 'magit-diff-mode-hook #'yura/magit-diff-rh-auto-set)
                 (remove-hook 'magit-log-mode-hook #'yura/magit-diff-rh-auto-set)
                 (setq yura/magit-diff-rh-auto-set-enable nil)
                 (message "Disable auto setting of the variable 'magit-diff-refine-hunk'."))
        (progn (add-hook 'magit-status-mode-hook #'yura/magit-diff-rh-auto-set)
               (add-hook 'magit-revision-mode-hook #'yura/magit-diff-rh-auto-set)
               (add-hook 'magit-diff-mode-hook #'yura/magit-diff-rh-auto-set)
               (add-hook 'magit-log-mode-hook #'yura/magit-diff-rh-auto-set)
               (setq yura/magit-diff-rh-auto-set-enable t)
               (message "Enable auto setting of the variable 'magit-diff-refine-hunk'."))))

    (defun yura/magit-diff-refine-hunk-toggle ()
      "Toggle value `magit-diff-refine-hunk' from list: nil, t, 'all."
      (interactive)
      (let* ((modes '(nil t all))
             (next (cadr (member magit-diff-refine-hunk modes))))
        (setq magit-diff-refine-hunk next)
        (message "Set 'magit-diff-refine-hunk': %s" magit-diff-refine-hunk)))

    (defhydra hydra-magit (:color teal
                           :hint nil)
      "
Magit:
^^^^     Status/Log                  ^^  Checkout/Find file               ^^ Repository      ^^^^       Misc                      ^^ Git Search     ^^  Refine-hunk
^^^^---------------------------------^^-----------------------------------^^-----------------^^^^---------------------------------^^----------------^^---------------------------
_s_/_g_: status                     _c_: checkout file(rewrite)         _f_: fetch           ^^    _!_: run                   _M-g_: in files      _t_: auto set refine-hunk(%(if yura/magit-diff-rh-auto-set-enable t nil))
^^  _l_: log current              _C-c_: find file(open new)            _P_: push            ^^    _$_: process buffer        _M-l_: in log        _T_: toggle refine-hunk(%(message \"%s\" magit-diff-refine-hunk))
_b_/_L_: log all/local branches     _F_: file-dispatch                  _h_: checkout        ^^    _m_: git-timemachine
^^_C-l_: log current buffer       _C-f_: find Git file                   ^^                  ^^  _M-r_: git checkout all
^^_C-o_: submodules list            _o_: submodule                       ^^                  _rc_/_rh_: reflog current/head
^^^^                                 ^^                                  ^^                  ^^  _M-b_: blame
"
      ("g" magit-status)
      ("s" magit-status)
      ("l" magit-log-current)
      ("b" magit-log-all-branches)
      ("L" magit-log-branches)
      ("C-l" magit-log-buffer-file)
      ("C-o" magit-list-submodules)

      ("c" magit-file-checkout)
      ("C-c" magit-find-file)
      ("F" magit-file-dispatch)
      ("C-f" counsel-git)
      ("o" magit-submodule)

      ("f" magit-fetch)
      ("P" magit-push)
      ("h" (lambda () (interactive)
             (call-interactively #'counsel-git-checkout)
             (magit-refresh-buffer)))

      ("!" magit-run)
      ("$" magit-process-buffer)
      ("m" git-timemachine)
      ("M-r" yura/git-checkout-all)
      ("rc" magit-reflog-current)
      ("rh" magit-reflog-head)
      ("M-b" magit-blame)

      ("M-g" counsel-git-grep)
      ("M-l" counsel-git-log)

      ("t" yura/magit-diff-refine-hunk-auto-setting-toggle :color red)
      ("T" yura/magit-diff-refine-hunk-toggle :color red)

      ("q"   nil "cancel")
      ("C-g" nil "cancel"))

    (use-package git-rebase
      :bind (:map git-rebase-mode-map
             ("C-j" . git-rebase-show-or-scroll-up)
             ;; Change the navigation with 'n' and 'p' to look like `git-log-mode', when these
             ;; bindings show the difference(maigit-diff)
             ("n". (lambda (arg) (interactive "P")
                     (forward-line arg) ;default binding to 'n'
                     (git-rebase-show-or-scroll-up)))
             ("p". (lambda (arg) (interactive "P")
                     (git-rebase-backward-line arg) ;default binding to 'p'
                     (git-rebase-show-or-scroll-up)))))

    ;; Enable 'git-commit-mode' yasnippet for the *COMMIT_EDITMSG* buffer
    ;; https://emacs.stackexchange.com/questions/27946/yasnippets-wont-expand-in-git-commit-mode
    (with-eval-after-load 'yasnippet
      (add-hook 'git-commit-mode-hook
                (lambda ()
                  (when (derived-mode-p 'text-mode)
                    (yas-activate-extra-mode 'git-commit-mode)))))

    ;; Some application are changing control characters(CRLF/LF),
    ;; `yura/git-checkout-all' revert this change.
    (defun yura/git-checkout-all (bool)
      "Git checkout all files, will launch 'git checkout --'."
      (interactive
       (list (progn (magit-status)
                    (y-or-n-p "Would you like checkout all files?"))))
      (if bool
          (magit-run-git "checkout" "--" ".")
        (message "Exit without checkout.")))

    ;; OpenSSH passphrase caching, via ssh-agent
    ;; https://github.com/magit/magit/wiki/Pushing-with-Magit-from-Windows
    ;; TODO: enable mini-buffer for OpenSSH passphrase
    (setenv "SSH_ASKPASS" "git-gui--askpass")

    ;; https://github.com/alphapapa/magit-todos
    (use-package magit-todos
      :commands (magit-todos-mode))))

;;; Magit-log
(use-package magit-log
  :init
  (progn
    ;; Set `magit-log-margin' value in :init as many other variables will be
    ;; dynamically set based on its value when `magit-log' is loaded.
    ;; (setq magit-log-margin '(t age magit-log-margin-width t 18)) ;Default value
    ;; Show the commit ages with 1-char time units
    ;;   minute->m, hour->h, day->d, week->w, month->M, year->Y
    ;; Also reduce the author column width to 11 as the author name is being
    ;; abbreviated below.
    (setq magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11)))
  :config
  (progn
    ;; Abbreviate author name. I added this so that I can view Magit log without
    ;; too much commit message truncation even on narrow screens (like on phone).
    (defun modi/magit-log--abbreviate-author (&rest args)
      "The first arg is AUTHOR, abbreviate it.
First Last  -> F Last
First.Last  -> F Last
Last, First -> F Last
First       -> First (no change).

It is assumed that the author has only one or two names."
      ;; ARGS               -> '((REV AUTHOR DATE))
      ;; (car ARGS)         -> '(REV AUTHOR DATE)
      ;; (nth 1 (car ARGS)) -> AUTHOR
      (let* ((author (nth 1 (car args)))
             (author-abbr (if (string-match-p "," author)
                              ;; Last, First -> F Last
                              (replace-regexp-in-string "\\(.*?\\), *\\(.\\).*" "\\2 \\1" author)
                            ;; First Last -> F Last
                            (replace-regexp-in-string "\\(.\\).*?[. ]+\\(.*\\)" "\\1 \\2" author))))
        (setf (nth 1 (car args)) author-abbr))
      (car args))                       ;'(REV AUTHOR-ABBR DATE)
    (advice-add 'magit-log-format-margin :filter-args #'modi/magit-log--abbreviate-author)

    ;; Limit the subject line to 50 characters
    (setq git-commit-summary-max-length 50)

    (defun yura/git-commit-mode-customization ()
      "Set Git message body limits to 72. https://chris.beams.io/posts/git-commit."
      (setq-local fill-column 72)
      (setq-local fci-rule-column 72))
    (add-hook 'git-commit-mode-hook #'yura/git-commit-mode-customization)))

;;; Git-timemachine
(use-package git-timemachine
  :config
  (progn
    (defun yura/git-timemachine-show-commit ()
      "Show commit for current revision, without changing the current window."
      (interactive)
      (git-timemachine-show-commit)
      (other-window -1))
    (defun yura/git-timemachine-show-previous-revision-commit ()
      "Show previous revision and commit of file, without changing the current window."
      (interactive)
      (git-timemachine-show-previous-revision)
      (yura/git-timemachine-show-commit))
    (defun yura/git-timemachine-show-next-revision-commit ()
      "Show next revision and commit of file, without changing the current window."
      (interactive)
      (git-timemachine-show-next-revision)
      (yura/git-timemachine-show-commit))

    ;; The `git-timemachine-help' is defined in `git-timemachine' package.
    ;; To add commands to `git-timemachine-help' I overwrite it.
    (transient-define-prefix git-timemachine-help ()
      "Show online help."
      ["Navigate"
       ("P" "show previous revision(show patch)" yura/git-timemachine-show-previous-revision-commit)
       ("N" "show next revision(show patch)" yura/git-timemachine-show-next-revision-commit)
       ("p" "show previous revision" git-timemachine-show-previous-revision)
       ("n" "show next revision" git-timemachine-show-next-revision)
       ("g" "show N-th revision" git-timemachine-show-nth-revision)
       ("t" "show fuzzy revision" git-timemachine-show-revision-fuzzy)]
      ["Kill current revision"
       ("w" "kill abbreviated revision" git-timemachine-kill-abbreviated-revision)
       ("W" "kill revision" git-timemachine-kill-revision)]
      ["Misc"
       ("b" "blame current revision" git-timemachine-blame)
       ("c" "show commit" yura/git-timemachine-show-commit)
       ("C" "show commit(and jump)" git-timemachine-show-commit)
       ("?" "show help" git-timemachine-help)
       ("q" "quit" git-timemachine-quit)])

    (bind-keys
     :map git-timemachine-mode-map
     ("c" . yura/git-timemachine-show-commit)
     ("C" . git-timemachine-show-commit)
     ("N" . yura/git-timemachine-show-next-revision-commit)
     ("P" . yura/git-timemachine-show-previous-revision-commit))))
;; Default key binding in `git-timemachine'
;; |---------+-------------------------------------------------------------------------|
;; | Binding | Command                                                                 |
;; |---------+-------------------------------------------------------------------------|
;; | p       | Visit previous historic version                                         |
;; | n       | Visit next historic version                                             |
;; | w       | Copy the abbreviated hash of the current historic version               |
;; | W       | Copy the full hash of the current historic version                      |
;; | g       | Goto nth revision                                                       |
;; | t       | Goto revision by selected commit message                                |
;; | q       | Exit the time machine.                                                  |
;; | b       | Run magit-blame on the currently visited revision (if magit available). |
;; | c       | Show current commit using magit (if magit available).                   |
;; |---------+-------------------------------------------------------------------------|

;;; Git-modes
;; The 'git-modes' package is not available from Melpa
(use-package git-modes
  :config
  (use-package gitconfig-mode
    :config
    (progn
      (with-eval-after-load 'easy-escape
        (add-hook 'gitconfig-mode-hook #'easy-escape-minor-mode)))))

;;; Emojify
;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :config
  (progn
    (dolist (hook '(git-commit-mode-hook
                    git-rebase-mode-hook
                    magit-diff-mode-hook
                    magit-status-mode-hook
                    magit-log-mode-hook))
      (add-hook hook #'emojify-turn-on-emojify-mode))))

;; https://github.com/dandavison/magit-delta
;; Requires the user to download the `delta' executable and put it somewhere
;; in the PATH before starting emacs.
;;
;; Due to a current issue in delta, you cannot enable its `line-numbers' feature without
;; messing up the diffs in magit. A workaround is that you can enable `side-by-side'
;; mode. That shows line numbers in side-by-side mode in the terminal, but leaves
;; `magit-delta' unaffected!
;;
;; [delta]
;;     # https://github.com/dandavison/magit-delta/issues/13
;;     # line-numbers = true    # Don't do this.. messes up diffs in magit
;;     #
;;     side-by-side = true      # Display a side-by-side diff view instead of the traditional view
(when (executable-find "delta")
  (use-package magit-delta
    :ensure t
    :hook (magit-mode . magit-delta-mode)))


(provide 'setup-magit)

;; |---------+----------------------------------|
;; | Binding | Description                      |
;; |---------+----------------------------------|
;; | j n     | Jump to Untracked section        |
;; | j u     | Jump to Unstaged section         |
;; | j s     | Jump to Staged section           |
;; | j p     | Jump to Unpushed section         |
;; | M-p     | Jump to previous sibling section |
;; | M-n     | Jump to next sibling section     |
;; |---------+----------------------------------|

;; TIPS:
;;
;;  Adding prefix to above jump commands also expands those sections and
;; brings that section to the top of the buffer.
;;   So `C-u j s' is analogous to doing `j s C-l C-l 4`
;;
;; Checkout the desired version of the file(overwrite file)
;;   1. Open file
;;   2. `magit-log-buffer-file' (hydra-magit, 'C-l')
;;   3. go to a desired commits
;;   4. `magit-file-checkout' (hydra-magit, 'c')
;;
;; Open the desired version of the file in a new buffer
;;   1. Open file
;;   2. `magit-log-buffer-file' (hydra-magit, 'C-l')
;;   3. go to a desired commits
;;   4. `magit-find-file' (hydra-magit/body, 'C-c')
;;
;; `magit-dispatch' 'C-x M-g'
;; `magit-file-dispatch' 'C-c M-g'
