;; Time-stamp: <2019-01-23 16:23:21 kmodi>

;; Contents:
;;
;;  Magit
;;  Magit-log
;;  Git-timemachine
;;  Git-modes

;;; Magit
;; https://github.com/magit/magit
(use-package magit
  :bind (:map modi-mode-map
         ("C-x g" . magit-status)
         ("C-c g". hydra-magit/body))
  :init
  ;; Mark the `yura/magit-diff-rh-auto-set-enable' variable as safe so that it can be
  ;; set in `.dir-locals.el' files or set in Local Variables.
  ;; Magit package loaded after use-package/commands,
  ;; so do not put a safe local variable using use-package/config
  (put 'yura/magit-diff-rh-auto-set-enable 'safe-local-variable #'booleanp)
  :commands (magit-status magit-log-all-branches)
  :config
  (progn
    (bind-keys
     :map magit-status-mode-map
     ("C-j" . magit-diff-show-or-scroll-up)
     :map magit-log-mode-map
     ("C-j" . magit-diff-show-or-scroll-up)
     :map magit-diff-mode-map
     ("C-j" . magit-diff-show-or-scroll-up)
     :map magit-cherry-mode-map
     ("C-j" . magit-diff-show-or-scroll-up))

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
^^^^     Status/Log                  ^^  Open Rev File                   ^^  Other                   ^^  Refine-hunk
^^^^---------------------------------^^----------------------------------^^--------------------------^^----------------------------
_s_/_g_: status                     _c_: checkout file(rewrite)         _W_: patch                  _t_: auto set refine-hunk(%(if yura/magit-diff-rh-auto-set-enable t nil))
^^  _l_: log current              _C-c_: find file(open new)            _!_: run                    _T_: toggle refine-hunk(%(message \"%s\" magit-diff-refine-hunk))
_b_/_L_: log all/local branches     _f_: file-dispatch                  _$_: process buffer
^^_C-l_: log current buffer          ^^                               _C-f_: find Git file
^^^^                                 ^^                                 _m_: git-timemachine
"
      ("g" magit-status)
      ("s" magit-status)
      ("l" magit-log-current)
      ("b" magit-log-all-branches)
      ("L" magit-log-branches)
      ("C-l" magit-log-buffer-file)

      ("c" magit-file-checkout)
      ("C-c" magit-find-file)
      ("f" magit-file-dispatch)

      ("W" magit-patch)
      ("!" magit-run)
      ("$" magit-process-buffer)
      ("t" yura/magit-diff-refine-hunk-auto-setting-toggle :color red)
      ("T" yura/magit-diff-refine-hunk-toggle :color red)
      ("C-f" counsel-git)

      ("m" git-timemachine)
      ("q"   nil "cancel" :color blue))

    (use-package git-rebase
      :bind (:map git-rebase-mode-map
             ("C-j" . git-rebase-show-or-scroll-up)))))

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
    (advice-add 'magit-log-format-margin :filter-args #'modi/magit-log--abbreviate-author)))

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
;; The package 'git-modes' do not available form Melpa
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode
  :mode (("\\.gitignore_global". gitignore-mode)))


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
