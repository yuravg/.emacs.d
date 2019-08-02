;; -*- mode: emacs-lisp -*-

;; make-files

(use-package make-mode
  :mode (("\\makefile\\'"        . makefile-mode)
         ("\\Makefile\\'"        . makefile-mode)
         ("\\Makefile\..+\\'"    . makefile-mode)
         ("\\Make.defines\\'"    . makefile-mode)
         ("\\Make.defines.in\\'" . makefile-mode)
         ("\\.inc\\'"            . makefile-mode))
  :mode (("\\GNUmakefile\\'" . makefile-gmake-mode ))
  :bind (:map makefile-mode-map
         ("C-j" . newline))  ;; Override the default binding to `compile-goto-error', compile.el
  :bind (:map makefile-gmake-mode-map
         ("C-j" . newline))) ;; Override the default binding to `compile-goto-error', compile.el


(provide 'setup-makefile)
