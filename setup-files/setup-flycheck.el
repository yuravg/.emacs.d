;; Time-stamp: <2018-05-17 14:44:23 kmodi>

;; Flycheck
;; https://github.com/flycheck/flycheck
;; http://www.flycheck.org/en/latest/index.html

(use-package flycheck
  :defer t
  :bind (:map modi-mode-map
         ("C-c e". hydra-flycheck/body))
  :config
  (progn
    (setq flycheck-mode-line-prefix "Ω")
    (defconst modi/flycheck-mode-hooks '(python-mode-hook
                                         emacs-lisp-mode-hook
                                         sh-mode-hook
                                         c++-mode-hook
                                         c-mode-hook
                                         ;; nim-mode-hook
                                         )
      "List of hooks of major modes in which flycheck mode should be enabled.")

    (defun modi/turn-on-flycheck-mode ()
      "Turn on flycheck-mode for the modes in `modi/flycheck-mode-hooks'."
      (interactive)
      (dolist (hook modi/flycheck-mode-hooks)
        (add-hook hook #'flycheck-mode)))

    (defun modi/turn-off-flycheck-mode ()
      "Turn off flycheck-mode for the modes in `modi/flycheck-mode-hooks'."
      (interactive)
      (dolist (hook modi/flycheck-mode-hooks)
        (remove-hook hook #'flycheck-mode)))

    (modi/turn-on-flycheck-mode)

    ;; Disable checkers
    ;; http://www.flycheck.org/en/latest/user/syntax-checkers.html
    ;; (setq-default flycheck-disabled-checkers '(sh-posix-dash))
    ;; (if (eq system-type 'windows-nt)
    ;;     (setq-default flycheck-disabled-checkers
    ;;                   (append flycheck-disabled-checkers '(verilog-verilator))))

    ;;; Flycheck Variables
    (setq flycheck-flake8-maximum-line-length 99)
    (setq flycheck-checker-error-threshold 800)

    ;; Enable C++ exceptions
    (add-to-list 'flycheck-clang-args "-fcxx-exceptions")

    ;;; Python checkers
    (use-package pylint
      :config
      ;; Messages:
      ;; http://stackoverflow.com/questions/4341746/how-do-i-disable-a-pylint-warning
      ;; http://pylint-messages.wikidot.com/all-messages
      ;; http://pylint.readthedocs.io/en/1.6.0/features.html
      (progn
        (autoload 'pylint "pylint")
        (add-hook 'python-mode-hook #'pylint-add-menu-items)
        (add-hook 'python-mode-hook #'pylint-add-key-bindings)

        ;; Enable pylint with flycheck:
        (add-to-list 'flycheck-checkers 'python-pylint)
        ;; simultaneous launch checkers:
        ;; https://github.com/flycheck/flycheck/issues/186
        (flycheck-add-next-checker 'python-flake8 'python-pylint)))

    ;;; C/C++ checkers
    ;;;; Google-cpplint (Google C++ Style checker), Clang
    ;; flycheck-google-cpplint - Google C++ Style checker for Flycheck.
    ;; https://github.com/flycheck/flycheck-google-cpplint/
    ;; http://google.github.io/styleguide/cppguide.html -- Google C++ Style Guide
    ;; https://github.com/google/styleguide
    ;; https://github.com/theandrewdavis/cpplint
    ;; Installation:
    ;; (install pip from https://pip.pypa.io/en/stable/installing/)
    ;;  pip install cpplint
    ;;  for checking installation you may run Lisp function: (executable-find "cpplint")
    ;;
    ;;;; Cppcheck
    ;; http://cppcheck.sourceforge.net/
    ;; Installation:
    ;;  - for Linux: something like 'sudo aptitude install cppcheck'
    ;;  - for Windows: download and install from http://cppcheck.sourceforge.net/
    ;; for checking installation you may run Lisp function: (executable-find "cppcheck")
    ;;
    ;;;; Clang:
    ;; Clang Compiler is an open-source compiler
    ;; http://clang.llvm.org/docs/UsersManual.html
    ;; Installation:
    ;;  - for Linux: something like 'sudo aptitude install clang'
    ;;  - for Windows: download and install from
    ;;     http://llvm.org/releases/download.html (Pre-Built Binaries, Clang for Windows (.sig))
    ;; for checking installation you may run Lisp function: (executable-find "clang")
    (defun check-command-is-executable (list)
      (let ((all-commad-executable t))
        (while list
          (let ((command (car list)))
            (when (not (executable-find command))
              (message "Can't find command: '%s'" command)
              (setq all-commad-executable nil)))
          (setq list (cdr list)))
        all-commad-executable))

    (defvar flycheck-cpp-checkers-list '("cpplint" "cppcheck" "clang") "\
List of C/C++ checkers to usage with Flycheck.")
    (defvar flycheck-cpp-checkers-installed nil "\
Non-nil if all C/C++ checkers installed.
`flycheck-cpp-checkers-list' - list of required checkers.")
    (if (check-command-is-executable flycheck-cpp-checkers-list)
        (setq flycheck-cpp-checkers-installed t)
      (message
       "Can't detect all requered C/C++ checkers(%s)."
       flycheck-cpp-checkers-list))

    (if flycheck-cpp-checkers-installed
        (use-package flycheck-google-cpplint
          :load-path "elisp/manually-synced/flycheck-google-cpplint"
          :config
          (progn
            (custom-set-variables
             '(flycheck-c/c++-googlelint-executable
               (executable-find "cpplint")))

            ;; Add Google C++ Style checker.
            ;; In default, syntax checked by Clang and Cppcheck.
            (flycheck-add-next-checker 'c/c++-cppcheck
                                       '(warning . c/c++-googlelint))
            ;; If you not use  cppcheck . You have need to change  flycheck-add-next-checker .
            ;; (flycheck-add-next-checker 'c/c++-clang
            ;;                         '(warning . c/c++-googlelint))
            (custom-set-variables
             '(flycheck-googlelint-verbose "1")
             '(flycheck-googlelint-filter "-whitespace/tab,
                                    -legal/copyright,
                                    -build/include_subdir,
                                    -build/header_guard,
                                    -build/include,
                                    -readability/todo")
             '(flycheck-googlelint-root "project/src")
             '(flycheck-googlelint-linelength "99"))
            ;; -whitespace/indent,
            ;; +whitespace/braces,
            ;; -whitespace,+whitespace/braces,
            )))

    (defhydra hydra-flycheck (:color blue
                              :columns 4)
      "Flycheck"
      ("l" flycheck-list-errors "list error" :color red)
      ("n" flycheck-next-error "next error")
      ("p" flycheck-previous-error "previous error")
      ("P" flycheck-pos-tip-mode "toggle error tip pop-up")
      ("u" flycheck-pos-tip-mode "toggle error tip pop-up")
      ("t" flycheck-mode "toggle flycheck mode" :color red)
      ("s" flycheck-select-checker "select checker")
      ("d" flycheck-disable-checker "disable checker")
      ("v" flycheck-verify-setup "verify setup"))

    ;;; Flycheck-pos-tip
    ;; https://github.com/flycheck/flycheck-pos-tip
    (use-package flycheck-pos-tip)))


(provide 'setup-flycheck)

;;; Notes
;; flycheck keymap prefix: "C-c !"
;; disable checker: flycheck-disable-checker
;; enable  checker: C-u flycheck-disable-checker
