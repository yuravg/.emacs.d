;; Time-stamp: <2022-04-11 08:51:42 kmodi>

;; Launcher

(defhydra hydra-launch (:color teal
                        :columns 6)
  "Launcher"
  ("a"       counsel-ag "ag cwd")
  ("b"       bookmark-jump "bookmark jump")
  ("cc"      calc "calc")
  ("cl"      hydra-literate-calc/body "calc literate")
  ("cg"      browse-url-chrome "google chrome")
  ("cr"      calendar "calendar")
  ("d"       dired-single-magic-buffer-current-dir "dired cwd")
  ("ed"      modi/ediff-dwim "ediff dwim")
  ("ee"      eww "eww")
  ("eb"      eww-list-bookmarks "eww bookmarks")
  ("el"      modi/eww-im-feeling-lucky "eww Lucky")
  ("eu"      (eww (browse-url-url-at-point)) "eww url at point")
  ("ev"      modi/eww-browse-url-of-file "eww current file")
  ("es"      eshell "eshell")
  ("ff"      browse-url-firefox "firefox")
  ("fv"      browse-url-of-file "firefox current file")
  ("g"       magit-status "magit status")
  ("h"       hl-line-flash "flash line")
  ("i"       counsel-git-grep "git grep")
  ("qc"      quick-calc "quick calc")
  ("rc"      rpn-calc "RPN calc")
  ("K"       modi/keep-lines-force "keep lines")
  ("l"        modi/run-current-file "load current file")
  ("L"       (modi/run-current-file 4) "load emacs init")
  ("m"       woman "man/woman")
  ("n"       neotree-toggle "neotree")
  ("o"       org-capture "org capture")
  ("p"       (call-interactively (if (package-installed-p 'paradox)
                                     #'paradox-list-packages ;Launch paradox if installed
                                   #'package-list-packages)) "packages")
  ;; chmod usage: s-SPC 644 P, s-SPC 400 P
  ("P"       modi/set-file-permissions "file permissions")
  ("r"       counsel-rg "ripgrep cwd")
  ("sa"      async-shell-command "shell async cmd")
  ("ss"      shell-command "shell cmd")
  ("se"      (sx-tab-all-questions nil "emacs") "emacs.SE")
  ("t"       shell "shell")
  ("C-t"     multi-term "terminal")
  ("u"       paradox-upgrade-packages "upgrade packages")
  ("w"       modi/weather "weather")
  ("<SPC>"   hydra-launch-freq/body "launch freq")
  ("<s-SPC>" hydra-launch-freq/body nil)
  (":"       eval-expression "eval")
  ("C-g"     nil "cancel"))

(defhydra hydra-launch-freq (:color teal
                             :columns 6)
  "Frequently accessed files"
  ("e" (find-file (expand-file-name "init.el" user-emacs-directory)) "emacs init")
  ("q" nil "cancel")
  ("C-g" nil "cancel"))

(bind-key "<s-SPC>" #'hydra-launch/body modi-mode-map)
;; Bind C-c SPC in global-map so that the org-mode-map binding is not
;; overridden.
(bind-key "C-c SPC" #'hydra-launch/body)
;; `C-c l' is bound to `org-store-link'.
(bind-key "C-c n" #'hydra-launch/body modi-mode-map)


(provide 'setup-launcher)
