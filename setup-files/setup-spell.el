;; Time-stamp: <2016-06-23 23:29:36 kmodi>
;;
;; Spell check
;; ispell, flyspell
;; aspell, hunspell
;;
;; NOTE: You need to have `aspell' or `hunspell' installed first
;;
;; Aspell Setup:
;; 1. Install aspell from http://aspell.net/
;;    - Install using ./configure --prefix=~/usr_local/bin, make, make install
;; 2. Download the latest dictionary from ftp://ftp.gnu.org/gnu/aspell/dict/0index.html
;;    and extract it.
;;    - Install the dictionary using ./configure, make, make install
;;
;; Hunspell Setup:
;; 1. Install hunspell from http://hunspell.sourceforge.net/(https://github.com/hunspell/hunspell)
;; 2. Download openoffice dictionary extension from
;;    http://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice
;;    http://extensions.openoffice.org/en/project/russian-dictionary
;;    http://extensions.openoffice.org/en/project/russian-dictionary-yo
;; 3. That is download `dict-en.oxt'. Rename that to `dict-en.zip' and unzip
;;    the contents to a temporary folder.
;; 4. Copy `en_US.dic' and `en_US.aff' files from there to a folder where you
;;    save dictionary files; I saved it to `~/usr_local/share/hunspell/'
;; 5. Add that path to shell env variable `DICPATH':
;;     setenv DICPATH $MYLOCAL/share/hunspell
;; 6. Restart emacs so that when hunspell is run by ispell/flyspell, that env
;;    variable is effective.
;;
;; hunspell will search for a dictionary called `en_US' in the path specified by
;; `$DICPATH'
;;
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html

(use-package ispell
  :if (not (bound-and-true-p disable-pkg-ispell))
  :defer 15
  :config
  (progn
    ;; http://www.emacswiki.org/emacs/InteractiveSpell
    (if (eq system-type 'windows-nt)
        (progn
          (setenv "DICTIONARY" "ru_RU")
          (setenv "DICTIONARY" "en_US")))
    (defun dictionary-ru()
      "Switch to the Russian dictionary."
      (interactive)
      (ispell-change-dictionary "ru_RU"))
    (defun dictionary-en ()
      "Switch to the English dictionary."
      (interactive)
      (ispell-change-dictionary "en_US"))
    (cond
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      (setq ispell-extra-args '("-d en_US"))
      ;; https://emacs.stackexchange.com/questions/21378/spell-check-with-multiple-dictionaries
      (setq ispell-dictionary "en_US,ru_RU")
      ;; ispell-set-spellchecker-params has to be called
      ;; before ispell-hunspell-add-multi-dic will work
      (ispell-set-spellchecker-params)
      (ispell-hunspell-add-multi-dic "en_US,ru_RU"))
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args   '("--sug-mode=ultra"
                                  "--lang=en_US"))))

    ;; Save a new word to personal dictionary without asking
    (setq ispell-silently-savep t)

    (use-package flyspell
      :init
      (progn
        ;; Below variables need to be set before `flyspell' is loaded.
        (setq flyspell-use-meta-tab nil)
        ;; Binding for `flyspell-auto-correct-previous-word'.
        (setq flyspell-auto-correct-binding (kbd "<S-f12>")))
      :config
      (progn
        (add-hook 'prog-mode-hook #'flyspell-prog-mode)
        (with-eval-after-load 'auto-complete
          (ac-flyspell-workaround))
        ;; https://github.com/larstvei/dot-emacs#flyspell
        (add-hook 'text-mode-hook #'turn-on-flyspell)
        (add-hook 'org-mode-hook  #'turn-on-flyspell)

        ;; Flyspell signals an error if there is no spell-checking tool is
        ;; installed. We can advice `turn-on-flyspell' and `flyspell-prog-mode'
        ;; to try to enable flyspell only if a spell-checking tool is available.
        (defun modi/ispell-not-avail-p (&rest args)
          "Return `nil' if `ispell-program-name' is available; `t' otherwise."
          (not (executable-find ispell-program-name)))
        (advice-add 'turn-on-flyspell   :before-until #'modi/ispell-not-avail-p)
        (advice-add 'flyspell-prog-mode :before-until #'modi/ispell-not-avail-p)

        ;; https://github.com/d12frosted/flyspell-correct
        (use-package flyspell-correct-ivy
          :config
          (use-package flyspell-correct
            :chords
            (("ww" . flyspell-correct-word-generic))))

        (defhydra hydra-flyspell (:color teal
                                  :hint nil)
          "
^^      Correct word:                 ^^GoTo:                           ^^Spell:
^^------------------------------------^^--------------------------------^^------
   _w_: ispell                     _n_: next error                   _b_: buffer
   _c_: flyspell
"
          ("w" ispell-word)
          ("c" flyspell-correct-word-generic)
          ("n" flyspell-goto-next-error)
          ("b" flyspell-buffer)
          ("q" nil "cancel"))
        (bind-keys
         :map modi-mode-map
         ("C-c s" . hydra-flyspell/body))

        (bind-keys
         :map flyspell-mode-map
         ;; Stop flyspell overriding other key bindings
         ("C-," . nil)
         ("C-." . nil)
         ("<C-f12>" . flyspell-goto-next-error))))))


(provide 'setup-spell)

;; How to add a new word to the dictionary?
;; 1. Run ispell-word when the cursor is over the word ( `M-$' )
;; 2. Press `i' to add the word to the dictionary
;; 3. Done!
;;
;; For `aspell', the new words are auto added to `~/.aspell.en.pws'.
;; For `hunspell', the new words are auto added to `~/.hunspell_en_US'.
;;
;; If the word does not auto-correct properly, call the function
;; `flyspell-auto-correct-previous-word' repeatedly till you find the
;; right match. It is easy if a key is bound to call that function.
