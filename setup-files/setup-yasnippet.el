;; Time-stamp: <2016-10-24 07:00:32 kmodi>

;; yasnippet
;; https://github.com/joaotavora/yasnippet

(use-package yasnippet
  :if (not (bound-and-true-p disable-pkg-yasnippet))
  :defer 20
  :bind (:map modi-mode-map
         ("s-y" . hydra-yas/body)
         ("C-c y" . hydra-yas/body))
  :chords (:map modi-mode-map
           ("YY" . yas-insert-snippet)
           ("yy" . yas-ido-expand))
  :config
  (progn
    (setq yas-prompt-functions '(yas-popup-isearch-prompt
                                 yas-ido-prompt
                                 yas-completing-prompt))

    (setq modi/yas-snippets-dir (let ((dir (concat user-emacs-directory
                                                   "snippets/")))
                                  (make-directory dir :parents)
                                  dir))

    ;; The directories listed in `yas-snippet-dirs' should contain snippet
    ;; folders only for the major modes where you are ever going to use
    ;; yasnippet.
    ;;   By default, `yas-snippet-dirs' also contains the snippets
    ;; directory that comes with the package, which contains major mode dirs
    ;; like `fundamental-mode' in which you are never going to use yasnippet!
    ;;   So the solution is to copy only the snippet folders that I am ever
    ;; going to use to `modi/yas-snippets-dir'.
    ;; (setq yas-snippet-dirs (list 'modi/yas-snippets-dir))

    (setq yas-new-snippet-default "# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}${3:
# binding: ${4:direct-keybinding}}${5:
# expand-env: ((yas-indent-line 'auto) (yas-also-auto-indent-first-line t) (yas-wrap-around-region t))}
# --
$0")

    (yas-global-mode 1)

    (defun modi/yas-org-path-to-caption (text)
      "Converts a file path in TEXT to caption.
Used in org-mode/figure-caption snippet.

Example: ./img/my-figure.png → My Figure"
      (capitalize
       (replace-regexp-in-string
        "[-_]" " " (replace-regexp-in-string
                    "\\(.*/\\)*\\(.*?\\)\\(\\..*\\)*" "\\2" text))))

    (defun modi/yas-org-caption-to-name (text)
      "Converts an org CAPTION in TEXT to NAME.
Used in org-mode/figure-caption and org-mode/table-caption snippets.

Example: My Figure → my_figure"
      (replace-regexp-in-string " " "_" (downcase text)))

    (defhydra hydra-yas (:color teal
                         :hint nil)
      "
Yasnippet:
^^     Insert               ^^  Library                    ^^   Update
^^--------------------------^^---------------------------------------------------------
  _i_: insert              _v_: visit snippet file         _C_: compile and reload all
  _I_: insert ido          _n_: new                        _r_: reload all
_C-i_: ivy                 _?_: list snippets
  _x_: expand
"
      ("i" yas-insert-snippet)
      ("I" yas-ido-expand)
      ("C-i" ivy-yasnippet)
      ("n" yas-new-snippet)
      ("v" yas-visit-snippet-file)
      ("r" yas-reload-all)
      ("C" yura/yas-recompile-and-reload)
      ("x" yas-expand)
      ("?" yas-describe-tables)
      ("q" nil "cancel" :color blue)
      ("C-g" nil "cancel" :color blue))

    (defun yura/yas-recompile-and-reload ()
      "Recompile and reload YASnippet.\n
Execute commands: `yas-recompile-all', `yas-reload-all'."
      (interactive)
      (progn
        (yas-recompile-all)
        (yas-reload-all)
        (message "Yasnippet has been recompiled and reloaded")))

    (use-package ivy-yasnippet
      :config
      (progn
        (setq ivy-yasnippet-expand-keys 'always) ;default is 'smart
        (defalias 'iy 'ivy-yasnippet)))

    ;; https://www.emacswiki.org/emacs/Yasnippet
    ;; Completing point by some yasnippet key
    (defun yas-ido-expand ()
      "Lets you select (and expand) a yasnippet key"
      (interactive)
      (let ((original-point (point)))
        (while (and
                (not (= (point) (point-min) ))
                (not
                 (string-match "[[:space:]\n]" (char-to-string (char-before)))))
          (backward-word 1))
        (let* ((init-word (point))
               (word (buffer-substring init-word original-point))
               (list (yas-active-keys)))
          (goto-char original-point)
          (let ((key (cl-remove-if-not
                      (lambda (s) (string-match (concat "^" word) s)) list)))
            (if (= (length key) 1)
                (setq key (pop key))
              (setq key (ido-completing-read "key: " list nil nil word)))
            (delete-char (- init-word original-point))
            (insert key)
            (yas-expand)))))

    ;; use popup menu for yas-choose-value
    ;; https://www.emacswiki.org/emacs/Yasnippet
    (use-package popup
      :config
      (progn
        ;; add some shotcuts in popup menu mode
        (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
        (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
        (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
        (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
        (define-key popup-menu-keymap (kbd "C-j") 'popup-select)
        (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)
        (define-key popup-menu-keymap (kbd "C-s") 'popup-isearch)

        (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
          (when (featurep 'popup)
            (popup-menu*
             (mapcar
              (lambda (choice)
                (popup-make-item
                 (or (and display-fn (funcall display-fn choice))
                     choice)
                 :value choice))
              choices)
             :prompt prompt
             ;; start isearch mode immediately
             :isearch t)))))

    ;; `yasnippets-quartus' package:
    ;; This package is downloaded manually(~/.emacs.d/yasnippets-quartus/), if it is needed.
    ;; `yasnippets-quartus' package has many snippets, load them only when they are needed:
    ;; - commands to enable snippets:
    ;;  `yasnippets-quartus-standard-enable', `yasnippets-quartus-professional-enable'
    ;; - commands to disable snippets:
    ;;  `yasnippets-quartus-standard-disable', `yasnippets-quartus-professional-disable'
    (use-package yasnippets-quartus
      :load-path (lambda () (list (expand-file-name "yasnippets-quartus" user-emacs-directory))))))


(provide 'setup-yasnippet)
