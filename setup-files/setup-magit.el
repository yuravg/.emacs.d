;; Time-stamp: <2019-01-23 16:23:21 kmodi>

;; magit
;; https://github.com/magit/magit

(use-package magit
  :bind (:map modi-mode-map
         ("C-x g" . magit-status)
         ("C-c g". hydra-magit/body))
  :commands (magit-status magit-log-all-branches)
  :config
  (progn
    (bind-keys
     :map magit-status-mode-map
     ("C-j" . magit-diff-show-or-scroll-up)
     :map magit-log-mode-map
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

    (defhydra hydra-magit (:color blue
                           :columns 4)
      "Magit"
      ("g" magit-status "status")
      ("s" magit-status "status")
      ("l" magit-log-all-branches "log")
      ("b" magit-branch-popup "branch popup")
      ("r" magit-rebase-popup "rebase popup")
      ("f" magit-fetch-popup "fetch popup")
      ("P" magit-push-popup "push popup")
      ("F" magit-pull-popup "pull popup")
      ("W" magit-format-patch "format patch")
      ("$" magit-process "process"))

    (use-package git-rebase
      :bind (:map git-rebase-mode-map
             ("C-j" . git-rebase-show-or-scroll-up)))))

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

;; Tip: Adding prefix to above jump commands also expands those sections and
;; brings that section to the top of the buffer.
;;   So `C-u j s' is analogous to doing `j s C-l C-l 4`
