;; Time-stamp: <2017-07-06 18:09:49 kmodi>

;; Python

;; Emacs built-in `python' mode
(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("\\.pyw\\'" . python-mode))
  :bind (:map python-mode-map
         ("<f9>" . python-shell-send-buffer))
  :config
  (progn
    (defvar modi/python-use-ipython t
      "When non-nil, use Ipython as the python interpreter instead of python3.")

    ;; Don't warn if guessing the indention fails, just set it to the value
    ;; of `python-indent-offset'.
    (setq python-indent-guess-indent-offset-verbose nil)

    ;; Change the default symbol prettification
    (setcdr (assoc "and" python--prettify-symbols-alist) ?&) ;Default ?^
    (setcdr (assoc "or" python--prettify-symbols-alist) ?|)  ;Default ?∨

    (if (and (executable-find "ipython")
             modi/python-use-ipython)
        (progn
          (setq python-shell-buffer-name "Ipython")
          (setq python-shell-interpreter "ipython")
          ;; https://emacs.stackexchange.com/q/24453/115
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25306
          (setq python-shell-interpreter-args "--simple-prompt -i"))
      (setq python-shell-interpreter "python3")) ;Default to python 3.x

    (defun yura/python-set-indentation ()
      "Customize the indentation for `python-mode'."
      (setq tab-width 4
            python-indent-offset 4
            indent-tabs-mode nil))
    (add-hook 'python-mode-hook #'yura/python-set-indentation)

;;; Elpy
    ;; https://github.com/jorgenschaefer/elpy
    ;; https://elpy.readthedocs.io/en/latest/index.html
    ;; https://realpython.com/blog/python/emacs-the-best-python-editor/
    (use-package elpy
      :config
      (progn
        (elpy-enable)

        ;; Disable Syntax checker flake8(flymake):
        ;; https://github.com/jorgenschaefer/elpy/issues/137
        (setq elpy-modules (remove 'elpy-module-flymake elpy-modules))

        ;; Elpy: Python shell prompts not detected #733
        ;; https://github.com/jorgenschaefer/elpy/issues/733
        (if (eq system-type 'windows-nt)
            (setq python-shell-prompt-detect-failure-warning nil))

        ;; Release "C-c C-f" for modi/fold-dwim:
        (unbind-key "C-c C-f" elpy-mode-map)   ; default bind: elpy-find-file
        (unbind-key "C-c C-f" python-mode-map) ; default bind: python-eldoc-at-point

        (bind-keys
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-j". company-complete-selection))))

;;; Anaconda-mode
    ;; https://github.com/proofit404/anaconda-mode
    ;; https://pypi.python.org/pypi/anaconda_mode
    ;;
    ;; Install: M-x package-install RET anaconda-mode RET,
    ;; for setup files to .emacs.d/anaconda-mode/0.x.x/
    ;; (prevent error message: No matching distribution found for anaconda-mode==0.x.x)
    ;;
    ;; Anaconda-mode require setup packages: json-rpc, jedi
    ;; installation: pip install jedi json-rpc --upgrade
    (use-package anaconda-mode
      :defer 10
      :config
      (progn
        ;; |------------+--------------------------------|
        ;; | Keybinding | Description                    |
        ;; |------------+--------------------------------|
        ;; | C-M-i      | anaconda-mode-complete         |
        ;; | M-.        | anaconda-mode-find-definitions |
        ;; | M-,        | anaconda-mode-find-assignments |
        ;; | M-r        | anaconda-mode-find-references  |
        ;; | M-*        | anaconda-mode-go-back          |
        ;; | M-?        | anaconda-mode-show-doc         |
        ;; |------------+--------------------------------|
        (add-hook 'python-mode-hook #'anaconda-mode)
        (add-hook 'python-mode-hook #'anaconda-eldoc-mode)))))


(provide 'setup-python)

;; | C-c C-p | Start the python shell        |
;; | C-c C-c | Send current buffer to python |
