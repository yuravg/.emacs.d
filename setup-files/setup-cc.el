
;; CC Mode

(use-package cc-mode
  :config
  (progn
    (defun yura/cc-set-indentation ()
      "Customize the indentation for `cc-mode'."
      (setq tab-width 4
            c-basic-offset 4
            indent-tabs-mode t))
    (add-hook 'c-mode-common-hook #'yura/cc-set-indentation)

    (defun cc-refactoring-of-alignment ()
      "Refactoring of alignment for C/C++ mode.

Refactoring of alignment for selected region, or for whole buffer if region don't selected."
      (interactive)
      (save-excursion
        (let ((beg (if (use-region-p) (region-beginning) (point-min)))
              (end (if (use-region-p) (region-end) (point-max))))
          (mapc (lambda (pair)
                  (let ((in-expr (car pair))
                        (out-expr (cdr pair)))
                    (goto-char beg)
                    (while (re-search-forward in-expr end :noerror)
                      (replace-match out-expr))))
                '(("(\\(\\s-+\\)"   . "(")
                  ("\\(\\s-+\\))"   . ")")
                  ("{\\(\\s-+\\)"   . "{")
                  ("\\(\\s-+\\)}"   . "}")
                  ("){"        . ") {")
                  ("(\\(\\s-+\\))"  . "()")
                  ("\\bif("     . "if (")
                  ("\\bfor("    . "for (")
                  ("\\bcase("   . "case (")
                  ("\\bdo{"     . "do {")
                  ("\\bswitch(" . "switch (")))
          (message "Refactoring of alignment is complete."))))

    (bind-keys
     :map c-mode-base-map
     ("M-q" . fill-paragraph)
     ("C-c C-c" . compile)
     ("C-c C-h" . ff-find-other-file))

    (use-package google-c-style-4t
      :load-path "elisp/manually-synced/google-c-style"
      :config
      (progn
        (add-hook 'c-mode-common-hook #'google-set-c-style-4t)
        (add-hook 'c-mode-common-hook #'google-make-newline-indent)))

    ;; Compile command
    ;; https://www.emacswiki.org/emacs/CompileCommand
    ;;
    ;; emulate make's .c.o implicit pattern rule, but with
    ;; different defaults for the CC, CPPFLAGS, and CFLAGS
    ;; variables:
    ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
    (defun yura/c-compile-command ()
      (unless (file-exists-p "Makefile")
        (set (make-local-variable 'compile-command)
             (let ((file (file-name-nondirectory buffer-file-name)))
               (format "%s -o %s.out %s %s %s"
                       (or (getenv "CC") "gcc")
                       (file-name-sans-extension file)
                       (or (getenv "CPPFLAGS") "-DDEBUG=9")
                       (or (getenv "CFLAGS") "-ansi -pedantic -Wall -Wextra -g")
                       file)))))
    (defun yura/c++-compile-command ()
      (unless (file-exists-p "Makefile")
        (set (make-local-variable 'compile-command)
             (let ((file (file-name-nondirectory buffer-file-name)))
               (format "%s -o %s.out %s %s %s"
                       (or (getenv "CC") "g++")
                       (file-name-sans-extension file)
                       (or (getenv "CPPFLAGS") "-DDEBUG=9")
                       (or (getenv "CFLAGS") "-g -Wall -lstdc++ -std=c++17")
                       file)))))

    (add-hook 'c-mode-hook #'yura/c-compile-command)
    (add-hook 'c++-mode-hook #'yura/c++-compile-command)))


(provide 'setup-cc)
