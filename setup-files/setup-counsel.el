;; Time-stamp: <2019-01-17 15:28:00 kmodi>

;; Counsel (comes packaged with the `swiper' package)

(use-package counsel
  :defer t
  :bind (:map modi-mode-map
         ("C-c C-w" . hydra-source-code-warnings/body))
  :init
  (progn
    ;; Do not bind the below keys to counsel commands if the user has decided
    ;; to use ido instead of ivy.
    (when (not (bound-and-true-p disable-pkg-ivy))
      (bind-keys
       :map modi-mode-map
       ("M-i" . yura/counsel-grep-or-swiper)
       ("ESC M-i" . swiper-isearch) ; Ivy 0.12.0: a faster swiper that's not line-based
       ("C-M-y" . counsel-yank-pop)
       ;; ("C-h F" . counsel-faces)       ;Overrides `Info-goto-emacs-command-node' ;C-h - bind backward-delete-char
       ;; ("C-h S" . counsel-info-lookup-symbol) ;C-h - bind backward-delete-char
       ("C-c u" . counsel-unicode-char)
       ("C-c C" . counsel-colors-emacs) ;Alternative to `list-colors-display'
       ([remap execute-extended-command] . counsel-M-x)
       ([remap bookmark-jump] . counsel-bookmark) ;Jump to book or set it if it doesn't exist, C-x r b
       ([remap bookmark-set] . counsel-bookmark)  ;C-x r m
       ([remap find-file]  . counsel-find-file)
       ([remap describe-bindings] . counsel-descbinds)
       ([remap finder-by-keyword] . counsel-package) ;C-h p
       ([remap describe-variable] . counsel-describe-variable)
       ([remap describe-function] . counsel-describe-function))
      (bind-keys
       ("M-o" . counsel-recentf))
      (bind-to-modi-map "v" #'counsel-set-variable)
      (bind-keys
       :map read-expression-map
       ("C-r" . counsel-expression-history)) ; useful in `eval-expression' (`M-:')
      (bind-chords
       ("JJ" . counsel-imenu)
       ("'/" . counsel-grep-or-swiper)
       (";'" . counsel-M-x))
      (with-eval-after-load 'org
        (bind-keys
         :map org-mode-map
         ("C-c C-q" . modi/counsel-org-tag))
        (bind-chords
         :map org-mode-map
         ("JJ" . counsel-org-goto)))    ;Jump to org headings
      (with-eval-after-load 'org-agenda
        (bind-key "C-c C-q" #'counsel-org-tag-agenda org-agenda-mode-map))))
  :commands (modi/counsel-org-tag)
  :config
  (progn
    ;; counsel-find-file
    (setq counsel-find-file-at-point t)
    (setq counsel-find-file-ignore-regexp
          (concat
           ;; file names beginning with # or .
           "\\(?:\\`[#.]\\)"
           ;; file names ending with # or ~
           "\\|\\(?:[#~]\\'\\)"))
    ;; Note that `ivy-extra-directories' should also not contain the "../" and
    ;; "./" elements if you don't want to see those in the `counsel-find-file'
    ;; completion list.
    (ivy-set-actions
     'counsel-find-file
     `(("x"
        (lambda (x) (delete-file (expand-file-name x ivy--directory)))
        ,(propertize "delete" 'face 'font-lock-warning-face))))

    ;; counsel-ag
    ;; Redefine `counsel-ag-base-command' with my required options, especially
    ;; the `--follow' option to allow search through symbolic links (part of
    ;; `modi/ag-arguments').
    ;; (setq counsel-ag-base-command "\\ag --vimgrep %s") ; default
    (setq counsel-ag-base-command
          ;; http://stackoverflow.com/a/12999828/1219634
          (concat (mapconcat #'identity
                             (append '("ag")
                                     modi/ag-arguments
                                     '("--noheading" ;No file names above matching content
                                       "--nocolor"))
                             " ")
                  " %s"            ;This MUST be %s, not %S
                                        ;https://github.com/abo-abo/swiper/issues/427
                  ))
    ;; Show parent directory in the prompt
    (ivy-set-prompt 'counsel-ag #'counsel-prompt-function-dir)

    ;; counsel-rg
    ;; Redefine `counsel-rg-base-command' with my required options, especially
    ;; the `--follow' option to allow search through symbolic links (part of
    ;; `modi/rg-arguments').
    (setq counsel-rg-base-command
          (concat (mapconcat #'identity
                             (append '("rg")
                                     modi/rg-arguments
                                     '("--no-heading" ;No file names above matching content
                                       "--color never"))
                             " ")
                  " %s ."            ;This MUST be %s, not %S
                                        ;https://github.com/abo-abo/swiper/issues/427
                  ))

    ;; counsel-grep
    ;; I use `counsel-grep' mainly via
    ;; `counsel-grep-or-swiper'. There, more often than not, I need
    ;; the search to be case-insensitive. But even better, I'd like to
    ;; do "Smart" about case-sensitively like ripgrep does. As grep
    ;; does not offer that option, I am using rg instead of grep in
    ;; `counsel-grep'.
    (setq counsel-grep-base-command ;Original value: "grep -E -n -e %s %s"
          (mapconcat #'identity
                     '("rg"
                       "--color=never"
                       "--line-number" ;Matches the grep -n switch in the original value
                       "--smart-case" ;Case-sensitive only when the searched expression has both cases
                       "--follow" ;Allows searching in symlinked files too
                       "--no-ignore"    ;Ignore the .ignore, .gitignore, etc.
                       "--no-ignore-global" ;Also ignore the ignore files from "global" sources
                       "--"        ;This marks the end of switches
                       "%s"        ;Placeholder for regular expression
                       "%s")       ;Placeholder for file name
                     " "))

    ;; counsel and org
    (defface modi/counsel-org-goto-level-1 '((t . (:inherit org-level-1 :weight normal)))
      "Face for Level 1 in `counsel-org-goto'.")
    (defface modi/counsel-org-goto-level-2 '((t . (:inherit org-level-2 :weight normal)))
      "Face for Level 2 in `counsel-org-goto'.")
    (defface modi/counsel-org-goto-level-3 '((t . (:inherit org-level-3 :weight normal)))
      "Face for Level 3 in `counsel-org-goto'.")
    (defface modi/counsel-org-goto-level-4 '((t . (:inherit org-level-4 :weight normal)))
      "Face for Level 4 in `counsel-org-goto'.")
    (defface modi/counsel-org-goto-level-5 '((t . (:inherit org-level-5 :weight normal)))
      "Face for Level 5 in `counsel-org-goto'.")
    (defface modi/counsel-org-goto-level-6 '((t . (:inherit org-level-6 :weight normal)))
      "Face for Level 6 in `counsel-org-goto'.")
    (defface modi/counsel-org-goto-level-7 '((t . (:inherit org-level-7 :weight normal)))
      "Face for Level 7 in `counsel-org-goto'.")
    (defface modi/counsel-org-goto-level-8 '((t . (:inherit org-level-8 :weight normal)))
      "Face for Level 8 in `counsel-org-goto'.")

    (setq counsel-org-goto-face-style 'custom)
    (setq counsel-org-goto-custom-faces '(modi/counsel-org-goto-level-1
                                          modi/counsel-org-goto-level-2
                                          modi/counsel-org-goto-level-3
                                          modi/counsel-org-goto-level-4
                                          modi/counsel-org-goto-level-5
                                          modi/counsel-org-goto-level-6
                                          modi/counsel-org-goto-level-7
                                          modi/counsel-org-goto-level-8))

;;; Update counsel-grep-or-swiper
    ;; Command `counsel-grep-or-swiper' is very handy for me.
    ;; I would like improve it by search in buffer's directory, search though directories,
    ;; search after select source directory. And make settings for frequently changed search
    ;; parameters.
    (defvar yura/counsel-rg-base-command counsel-rg-base-command "Variable to store \
default value of `counsel-rg-base-command'.")
    (defvar yura/counsel-rg-base-command-hidden nil "Options 'Search hidden files \
and directories' for command `counsel-rg-base-command'.\n
For additional information see at RipGrep help, options: --hidden.")
    (defvar yura/counsel-rg-base-command-noignore nil "Options 'Do not respect ignore files' \
for command `counsel-rg-base-command'.\n
For additional information see at RipGrep help, options: --no-ignore.")
    (defvar yura/counsel-rg-base-command-iglob nil "Include or exclude files and directories
for searching that match the given glob' for command `counsel-rg-base-command'.\n
For additional information see at RipGrep help, options: --iglob.")
    (defvar yura/counsel-rg-base-command-maxdepth nil "Limit the depth of directory traversal\
to NUM levels beyond the paths given,\nfor command `counsel-rg-base-command', nil - no limit.
For additional information see at RipGrep help, options: --max-depth.")

    (defun yura/counsel-rg-base-command-reinit ()
      "Set `counsel-rg-base-command' to default."
      (interactive)
      (setq counsel-rg-base-command yura/counsel-rg-base-command
            yura/counsel-rg-base-command-hidden nil
            yura/counsel-rg-base-command-noignore nil
            yura/counsel-rg-base-command-maxdepth nil
            yura/counsel-rg-base-command-iglob nil))

    (defun yura/counsel-rg-base-command-setup ()
      "Setup `counsel-rg-base-command'.

Setup by variables:
`yura/counsel-rg-base-command-hidden',
`yura/counsel-rg-base-command-noignore'
`yura/counsel-rg-base-command-maxdepth'
`yura/counsel-rg-base-command-iglob'."
      (interactive)
      (let ((cmd yura/counsel-rg-base-command)
            (hidden yura/counsel-rg-base-command-hidden)
            (noignore yura/counsel-rg-base-command-noignore)
            (maxdepth yura/counsel-rg-base-command-maxdepth)
            (iglob yura/counsel-rg-base-command-iglob))
        (if hidden
            (if (string-match " --no-heading" cmd)
                (setq cmd (replace-regexp-in-string " --no-heading" " --hidden" cmd))
              (setq cmd (concat cmd " --hidden"))))
        (if noignore
            (if (string-match " --ignore-file" cmd)
                (setq cmd (replace-regexp-in-string " --ignore-file" " --no-ignore --ignore-file" cmd))
              (setq cmd (concat cmd " --no-ignore"))))
        (if maxdepth
            (setq cmd (concat cmd " --maxdepth " maxdepth)))
        (if iglob
            (setq cmd (concat cmd " --iglob " iglob )))
        (setq counsel-rg-base-command cmd)))

    (defun yura/counsel-rg-bcmd-hidden-toggle-update ()
      "Toggle variable yura/counsel-rg-base-command-hidden, update variable `counsel-rg-base-command'."
      (interactive)
      (progn
        (setq yura/counsel-rg-base-command-hidden
              (if yura/counsel-rg-base-command-hidden nil t))
        (yura/counsel-rg-base-command-setup)))

    (defun yura/counsel-rg-bcmd-noignore-toggle-update ()
      "Toggle variable yura/counsel-rg-base-command-noignore, update variable `counsel-rg-base-command'."
      (interactive)
      (progn
        (setq yura/counsel-rg-base-command-noignore
              (if yura/counsel-rg-base-command-noignore nil t))
        (yura/counsel-rg-base-command-setup)))

    (defun yura/counsel-rg-bcmd-maxdepth-set-update (depth)
      "Set variable yura/counsel-rg-base-command-maxdepth, update variable `counsel-rg-base-command'."
      (interactive
       (list (y-or-n-p "Set depth? ")))
      (if depth
          (setq yura/counsel-rg-base-command-maxdepth (read-string "Max-depth: "))
        (setq yura/counsel-rg-base-command-maxdepth nil))
      (yura/counsel-rg-base-command-setup))

    (defun yura/counsel-rg-bcmd-iglob-set-update (set-mask)
      "Set variable yura/counsel-rg-base-command-iglob, update variable `counsel-rg-base-command'."
      (interactive
       (list (y-or-n-p "Set mask? ")))
      (if set-mask
          (setq yura/counsel-rg-base-command-iglob (read-string "Glob mask: "))
        (setq yura/counsel-rg-base-command-iglob nil))
      (yura/counsel-rg-base-command-setup))

    (defun yura/counsel-rg-compare-bcmds ()
      "Returns the boolean comparison result for variables
`counsel-rg-base-command' and `yura/counsel-rg-base-command'."
      (interactive)
      (if (string-match
           (replace-regexp-in-string " %s" "" counsel-rg-base-command)
           (replace-regexp-in-string " %s" "" yura/counsel-rg-base-command))
          t
        nil))

    (defhydra hydra-counsel-rg-base-command (:color red :hint nil)
      "
[Customize command: counsel-rg-base-command (%(replace-regexp-in-string \" %s\" \"\" counsel-rg-base-command))]
^^       Options
  ----------------------------------------------
  _h_: search hidden files and directories (%(if yura/counsel-rg-base-command-hidden t nil))
  _i_: don't respect ignore files          (%(if yura/counsel-rg-base-command-noignore t nil))
  _g_: iglob(no case) mask, set or clean   (%(message yura/counsel-rg-base-command-iglob))
  _d_: max-depth                           (%(message yura/counsel-rg-base-command-maxdepth))
  _r_: set to default                      (%(if (yura/counsel-rg-compare-bcmds) \"default\" \"changed\"))

"
      ("h" yura/counsel-rg-bcmd-hidden-toggle-update)
      ("i" yura/counsel-rg-bcmd-noignore-toggle-update)
      ("g" yura/counsel-rg-bcmd-iglob-set-update)
      ("d" yura/counsel-rg-bcmd-maxdepth-set-update)
      ("r" yura/counsel-rg-base-command-reinit)
      ("q" nil "cancel")
      ("C-g" nil "cancel"))
    (defalias 'rgs 'hydra-counsel-rg-base-command/body
      "Setup command `counsel-rg-base-command'")

    (defun yura/counsel-grep-or-swiper (&optional arg)
      "Call `counsel-grep-or-swiper' or `counsel-rg'.

Without ARG call `counsel-grep-or-swiper'
If ARG is \\[universal-argument] \t\t\t\t call `counsel-rg' for current buffer directory with minimal depth.
If ARG is \\[universal-argument] \\[universal-argument] \t\t\t call `counsel-rg' for current buffer directory.
If ARG is \\[universal-argument] \\[universal-argument] \\[universal-argument] \t\t call `counsel-rg' for selected directory.
If ARG is \\[universal-argument] \\[universal-argument] \\[universal-argument] \\[universal-argument]\
\t call `counsel-rg' unrestricted search at selected directory,\n with 'ripgrep' options: -uuu."
      (interactive "p")
      (cl-case arg
        (4   (counsel-rg nil (file-name-directory buffer-file-name) " --maxdepth 1"))
        (16  (counsel-rg nil (file-name-directory buffer-file-name) ""))
        (64  #'lambda () (let ((dir (read-directory-name "Starting directory: " nil default-directory t)))
                      (counsel-rg nil dir "")))
        (256 #'lambda () (let ((dir (read-directory-name "Starting directory: " nil default-directory t)))
                      (counsel-rg nil dir " -uuu")))
        (t (counsel-grep-or-swiper))))

    ;; TODO: use ivy-set-actions with yura/counsel-grep-or-swiper
    (ivy-set-actions 'yura/counsel-grep-or-swiper nil)
    (ivy-set-actions
     'yura/counsel-grep-or-swiper
     '(("s"
        (lambda(x) (hydra-counsel-rg-base-command/body))
        "Setup")))

    (defun yura/counsel-prompt-function-dir ()
      "Return prompt appended with the parent directory.

Modified version of `counsel-prompt-function-dir'."
      (require 'esh-util)
      (let* ((dir (ivy-state-directory ivy-last))
             (parts (nthcdr 3 (eshell-split-path dir)))
             (dir (format " [%s]: %s"
                          (if parts (apply #'concat "..." parts) dir)
                          (if (yura/counsel-rg-compare-bcmds)
                              ""
                            (concat
                             "(options:"
                             (if yura/counsel-rg-base-command-hidden " hidden")
                             (if yura/counsel-rg-base-command-noignore " noignore")
                             (if yura/counsel-rg-base-command-iglob
                                 (message " %s" yura/counsel-rg-base-command-iglob))
                             (if yura/counsel-rg-base-command-maxdepth
                                 (message " maxdepth=%s" yura/counsel-rg-base-command-maxdepth))
                             "): ")))))
        (ivy-add-prompt-count
         (replace-regexp-in-string          ; Insert dir before any trailing colon.
          "\\(?:: ?\\)?\\'" dir (ivy-state-prompt ivy-last) t t))))

    (ivy-set-prompt 'counsel-rg #'yura/counsel-prompt-function-dir)

;;; Source code warnings navigation
    (defvar yura/src-warning-expression "\\(FIXME\\|TODO\\)"
      "Expression to search for source code warnings.")

    (defun yura/buffer-src-warning ()
      "Search for source code warnings in current buffer.
Search for `yura/src-warning-expression' with `counsel-grep-or-swiper'."
      (interactive)
      (counsel-grep-or-swiper yura/src-warning-expression))

    (defun yura/buffer-dir-src-warning ()
      "Search for source code warnings in current buffer's directory(including all subdirectories).
Search for `yura/src-warning-expression' with `counsel-rg' in buffer's directory."
      (interactive)
      (counsel-rg yura/src-warning-expression
                  (file-name-directory buffer-file-name)))

    (defun yura/buffer-dir-only-src-warning ()
      "Search for source code warnings in current buffer's directory only(with minimal depth).
Search for `yura/src-warning-expression' with `counsel-rg' in buffer's directory."
      (interactive)
      (counsel-rg yura/src-warning-expression
                  (file-name-directory buffer-file-name) " --maxdepth 1"))

    (defun yura/projectile-src-warning ()
      "Search for source code warnings in projectile root directory.
Search for `yura/src-warning-expression' with `counsel-rg' in `projectile-project-root'."
      (interactive)
      (counsel-rg yura/src-warning-expression
                  (projectile-project-root)))

    (defun yura/selected-dir-only-src-warning ()
      "Search for source code warnings in selected directory(with minimal depth).
Search for `yura/src-warning-expression' with `counsel-rg' in selected directory."
      (interactive)
      (let ((dir (read-directory-name "In directory: "
                                      nil default-directory t)))
        (counsel-rg yura/src-warning-expression dir " --maxdepth 1")))

    (defun yura/selected-dir-src-warning ()
      "Search for source code warnings in selected directory(including all subdirectories).
Search for `yura/src-warning-expression' with `counsel-rg' in selected directory."
      (interactive)
      (let ((dir (read-directory-name "In directory: "
                                      nil default-directory t)))
        (counsel-rg yura/src-warning-expression dir)))

    (defhydra hydra-source-code-warnings (:color teal
                                          :hint nil)
      "
Show source code warnings(%(message yura/src-warning-expression)) for:

      ^^_b_: buffer
    _d_/_D_: buffer's directory(only/subdir): %(file-name-directory buffer-file-name)
      ^^_p_: projectile                 root: %(if (fboundp 'projectile-project-root) (projectile-project-root) \"TBD\")
    _s_/_S_: after selecting a directory(only in/with subdir)
    "
      ("b" yura/buffer-src-warning)
      ("s" yura/selected-dir-only-src-warning)
      ("S" yura/selected-dir-src-warning)
      ("d" yura/buffer-dir-only-src-warning)
      ("D" yura/buffer-dir-src-warning)
      ("p" yura/projectile-src-warning)
      ("q" nil "cancel")
      ("C-g" nil "cancel"))

    ;; Counsel and Org tags
    (defun modi/counsel-org-tag (&optional option)
      "Set Org tags, or just align tags in the whole buffer.

If OPTION is non-nil, call `org-set-tags-command' with the same
OPTION value.

Else call `counsel-org-tag'."
      (interactive "P")
      (if option
          (org-set-tags-command option)
        (counsel-org-tag))))

;;; counsel-projectile
  (use-package counsel-projectile))


(provide 'setup-counsel)

;; TIPS
;;
;; (1) Refactoring power-ups
;; http://manuel-uberti.github.io/emacs/2018/02/10/occur/
;; https://oremacs.com/2017/11/30/ivy-0.10.0/
;; 1. 'C-c C-r' brings up `counsel-projectile-rg'; type in
;; 2. 'C-c C-o' now runs `ivy-occur' and all the candidates end up in a dedicated buffer
;;    'C-d' to delete uninteresting lines
;; 3. 'w' bound to `ivy-wgrep-change-to-wgrep-mode', makes the new buffer editable.
;; 4. 'C-c r' bind to `anzu-query-replace'
;; 5. 'C-c C-c' to confirm modification('C-c C-k' to abort changes)
;; 6. 'C-x s' bind to `save-some-buffers'
;;
;; Peek at files with `C-M-n' and `C-M-p'
;; Input a leading dot to see all files
;;
;; Related blog posts:
;; - http://oremacs.com/2015/06/08/describe-variable-tip
