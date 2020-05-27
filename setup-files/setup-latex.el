;; Time-stamp: <2018-02-20 15:00:44 kmodi>
;;
;; LaTeX
;;
;; https://www.tug.org/levels.html
;; https://www.emacswiki.org/emacs/AUCTeX
;; http://tug.org/texlive
;;
;; More documentation:
;;   M-x info RET preview-latex
;; Detailed documentation for the LaTeX style(from shell):
;;   $ texdoc preview
;;

(use-package tex
  :defer 10
  :config
  (progn
    (load "auctex.el" nil t t)
    (load "preview.el" nil t t)

    ;; xelatex - TeX Live
    (if (executable-find "xelatex")
        (progn
          (setq LaTeX-command "xelatex -shell-escape")
          (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
          (setq TeX-command-default "XeLaTeX"))
      (setq LaTeX-command "latex -shell-escape"))

    (setq TeX-PDF-mode   t)
    (setq TeX-auto-save  t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil)
    ;; http://www.gnu.org/software/auctex/manual/auctex/Multifile.html
    (setq-default TeX-master nil))) ; Query for master file.


(provide 'setup-latex)
