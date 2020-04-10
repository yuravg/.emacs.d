;; Time-stamp: <2018-02-20 15:00:44 kmodi>
;;
;; LaTeX
;;
;; NOTE: auctex has to be installed from outside emacs for the below `load's
;; to work.
;;
;; 1. Download the latest auctex from http://www.gnu.org/software/auctex/download-for-unix.html
;; 2. tar xvzf auctex-VERSION.tar.gz
;; 3. ./configure --prefix=/home/kmodi/usr_local --with-lispdir=/home/kmodi/.emacs.d/auctex/ --with-texmf-dir=/home/kmodi/texlive/texmf-local/
;;    - prefix <- Location of your /usr/local
;;    - with-lispdir <- I prefer to keep auctex elisp stuff in my ~/.emacs.d
;;    - with-texmf-dir <- Location of texlive texmf directory
;; 4. make
;; 5. make install

(use-package tex
  :config
  (progn
    (defvar auctex-install-dir (file-name-as-directory (expand-file-name "auctex" user-emacs-directory))
      "AucTeX install directory.")

    (when (file-exists-p auctex-install-dir)
      (add-to-list 'load-path auctex-install-dir)
      (load "auctex.el" nil t t)
      (load "preview-latex.el" nil t t))

    (if (executable-find "xelatex")
        (setq LaTeX-command "xelatex -shell-escape")
      (setq LaTeX-command "latex -shell-escape"))

    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
    (setq TeX-command-default "XeLaTeX")

    ;; http://www.gnu.org/software/auctex/manual/auctex/Multifile.html
    (setq TeX-PDF-mode   t)
    (setq TeX-auto-save  t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil)

    (setq-default TeX-master nil))) ; Query for master file.


(provide 'setup-latex)
