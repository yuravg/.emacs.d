;; Time-stamp: <2018-08-23 18:00:37 kmodi>

;; Deadgrep
;; https://github.com/Wilfred/deadgrep

(use-package deadgrep
  :ensure t
  :defer t
  :init
  (progn
    (bind-to-modi-map "R" #'deadgrep)) ;default binding to "r"
  :config
  (progn
    (setq-default deadgrep--search-type 'regexp) ;Default is 'string

    (defconst modi/deadgrep--rg-extra-args
      `("--no-ignore-vcs"       ;Ignore files/dirs ONLY from `.ignore'
        "--follow"              ;Follow symlinks
        "--ignore-file" ,(expand-file-name ".ignore" (getenv "HOME")))
      "Extra rg arguments to be added to `deadgrep--format-command' output.")

    ;; Thu Aug 23 15:51:37 EDT 2018 - kmodi
    ;; Adding extra arguments to the ones
    ;; already in `deadgrep--format-command' --
    ;; https://github.com/Wilfred/deadgrep/issues/24
    (defun modi/deadgrep--format-command-advice (orig-ret-val)
      "Add arguments from `modi/deadgrep--rg-extra-args' to ORIG-RET-VAL."
      (let ((new-ret (replace-regexp-in-string
                      (format "\\`\\(%s \\)\\(.*\\)\\'" (regexp-quote deadgrep-executable))
                      (concat "\\1"
                              (mapconcat #'identity modi/deadgrep--rg-extra-args " ")
                              " \\2")
                      orig-ret-val)))
        (message "[Deadgrep] %s" new-ret)
        new-ret))
    (advice-add 'deadgrep--format-command :filter-return #'modi/deadgrep--format-command-advice)

    (defun modi/deadgrep--jump-to-and-execute (re)
      "Execute the button that matches RE and push it."
      (goto-char (point-min))
      (re-search-forward re)
      (backward-char 3)
      (push-button))

    (defun modi/deadgrep-change-search-term ()
      "Change the search term."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Search term: .*change$"))

    (defun modi/deadgrep-change-search-type-to-string ()
      "Change the search type to 'string'."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Search type: .*string"))

    (defun modi/deadgrep-change-search-type-to-words ()
      "Change the search type to 'words'."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Search type: .*words"))

    (defun modi/deadgrep-change-search-type-to-regexp ()
      "Change the search type to 'regexp'."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Search type: .*regexp"))

    (defun modi/deadgrep-change-case-to-smart ()
      "Change the case sensitivity to 'smart'."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Case: .*smart"))

    (defun modi/deadgrep-change-case-to-sensitive ()
      "Change the case sensitivity to 'sensitive'."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Case: .*sensitive"))

    (defun modi/deadgrep-change-case-to-ignore ()
      "Change the case sensitivity to 'ignore'."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Case: .*ignore"))

    (defun modi/deadgrep-change-context-to-none ()
      "Don't show ny context around the search results."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Context: .*none"))

    (defun modi/deadgrep-change-context-to-before ()
      "Set 'before' context for the search results."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Context: .*before"))

    (defun modi/deadgrep-change-context-to-after ()
      "Set 'after' context for the search results."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Context: .*after"))

    (defun modi/deadgrep-change-directory ()
      "Change the root directory for searches."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Directory: .*$"))

    (defun modi/deadgrep-search-all-files ()
      "Change file search scope to 'all'."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Files: .*all"))

    (defun modi/deadgrep-search-files-by-type ()
      "Search only in the specified file types."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Files: .*type"))

    (defun modi/deadgrep-search-files-by-glob ()
      "Search in files names that match the specified glob."
      (interactive)
      (modi/deadgrep--jump-to-and-execute "^Files: .*glob"))

    (bind-keys
     :map deadgrep-mode-map
     ("s" . modi/deadgrep-change-search-term)
     ("ts" . modi/deadgrep-change-search-type-to-string)
     ("tw" . modi/deadgrep-change-search-type-to-words)
     ("tr" . modi/deadgrep-change-search-type-to-regexp)
     ("cs" . modi/deadgrep-change-case-to-smart)
     ("cc" . modi/deadgrep-change-case-to-sensitive)
     ("ci" . modi/deadgrep-change-case-to-ignore)
     ("xn" . modi/deadgrep-change-context-to-none)
     ("xb" . modi/deadgrep-change-context-to-before)
     ("xa" . modi/deadgrep-change-context-to-after)
     ("d" . modi/deadgrep-change-directory)
     ("fa" . modi/deadgrep-search-all-files)
     ("ft" . modi/deadgrep-search-files-by-type)
     ("fg" . modi/deadgrep-search-files-by-glob))))

;; https://github.com/dajva/rg.el
(use-package rg
  :init
  (progn
    (bind-to-modi-map "r" #'rg)
    (bind-to-modi-map "C-r" #'rg-project))
  :config
  (progn
    (defvar yura/rg-truncate-lines t "Variable `truncate-lines' for `rg-mode'.")

    (defun yura/rg-toggle-truncate-lines ()
      "Toggle `yura/rg-truncate-lines'."
      (interactive)
      (progn
        (setq yura/rg-truncate-lines
              (if (bound-and-true-p yura/rg-truncate-lines)
                  nil t))
        (message "Truncate line (rg-mode): %s" yura/rg-truncate-lines)))

    (defun yura/toggle-truncate-lines-rg-mode-hook ()
      (setq truncate-lines yura/rg-truncate-lines))
    (add-hook 'rg-mode-hook #'yura/toggle-truncate-lines-rg-mode-hook)

    (defhydra hydra-rg (:color blue
                        :hint nil)
      "
[rg mode]
  _s_/_t_: search literal                  _d_: directory              _T_: toggle truncate
    ^^_r_: search regexp                   _f_: file pattern           _l_: list searches
    ^^_c_: toogle case insensitive         _i_: toggle ignore          _S_: save search result
"
      ("s" rg-rerun-change-literal)
      ("t" rg-rerun-change-literal)
      ("r" rg-rerun-change-regexp)
      ("c" rg-rerun-toggle-case)
      ("d" rg-rerun-change-dir)
      ("f" rg-rerun-change-files)
      ("i" rg-rerun-toggle-ignore)
      ("T" yura/rg-toggle-truncate-lines)
      ("S" rg-save-search-as-name)
      ("l" rg-list-searches)
      ("q" nil "cancel" :color blue)
      ("C-g" nil "cancel" :color blue))

    (bind-keys
     :map rg-mode-map
     ("?" . hydra-rg/body))))

;; Projectile mode uses `ripgrep' by default for `projectile-ripgrep'
(use-package ripgrep)


(provide 'setup-rg)
