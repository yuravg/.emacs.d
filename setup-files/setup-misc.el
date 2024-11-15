;; Time-stamp: <2019-07-30 13:50:27 kmodi>

;; Miscellaneous config not categorized in other setup-* files

(fset 'yes-or-no-p 'y-or-n-p)           ;Use y or n instead of yes or no

;; Delete stuff to a trash directory
(setq delete-by-moving-to-trash t)
(setq trash-directory (let ((dir (file-name-as-directory (expand-file-name ".trash_emacs" modi/temporary-file-directory))))
                        (make-directory dir :parents)
                        dir))

;; Uncompress->edit->save->compress .gz, .bz2, .Z files on the fly
(auto-compression-mode 1)

(use-package executable
  :config
  (progn
    (>=e "26.0"
        (setq executable-prefix-env t)) ;Use "#!/usr/bin/env python3" style magic number

    ;; On saving, automatically make a file an executable if it begins with "#!"
    ;; Examples: #!/usr/bin/env bash
    ;;           #!/usr/bin/env perl
    (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)))

(use-package sendmail
  :defer t
  :config
  (progn
    ;; Send mail using `sendmail' package
    (setq send-mail-function #'sendmail-send-it)))

;; url
(use-package url
  :defer t
  :preface
  (progn
    ;; Solve the issue with `sx.el' when using that package simultaneously in
    ;; different emacs versions
    (setq url-configuration-directory
          (let ((dir (concat user-emacs-directory
                             "url_" emacs-version-short "/")))
            (make-directory dir :parents)
            dir))))

(use-package vc-hooks
  :config
  (progn
    ;; Don't ask if I want to visit a sym-linked file under VC. I always want to!
    (setq vc-follow-symlinks t)))

;; Execute the script in current buffer
;; http://ergoemacs.org/emacs/elisp_run_current_file.html
(defun modi/run-current-file (&optional eval-init)
  "Execute the current file.

If EVAL-INIT is non-nil, load the `init.el'.

For example, if the current buffer is the file xx.py, then it'll call
“python xx.py” in a shell. The file can be php, perl, python, ruby,
javascript, bash, ocaml, vb, elisp.  File extension is used to determine
what program to run.

If the file is modified, user is asked to save it first.

If the buffer major-mode is `emacs-lisp-mode', run `eval-buffer'.
If the buffer major-mode is `clojure-mode', run `cider-load-buffer'."
  (interactive "P")
  (if eval-init
      (load (locate-user-emacs-file "init.el"))
    (progn
      (when (buffer-modified-p)
        (when (y-or-n-p "Buffer modified. Do you want to save first?")
          (save-buffer)))

      (cond
       ((derived-mode-p 'emacs-lisp-mode)
        (eval-buffer)                   ;Also works for .el.gz and .el.gpg files
        (message "Evaluated `%0s'." (buffer-name)))
       ((and (featurep 'cider)
             (derived-mode-p 'clojure-mode))
        (cider-load-buffer))
       (t
        (let* ((suffix-map '(("py" . "python")
                             ("rb" . "ruby")
                             ("sh" . "bash")
                             ("csh" . "tcsh")
                             ("pl" . "perl")
                             ("tex" . "pdflatex")
                             ("latex" . "pdflatex")
                             ("d" . "dmd -de -w -unittest -run")
                             ("nim" . "nim c -r --verbosity:0")))
               (file-name (progn
                            ;; Save buffer as a file if it's not already a file.
                            (when (null (buffer-file-name)) (save-buffer))
                            (buffer-file-name)))
               (file-ext (file-name-extension file-name))
               (prog-name (cdr (assoc file-ext suffix-map)))
               (cmd-str (concat prog-name " \"" file-name "\"")))
          (if prog-name
              (progn
                ;; (view-echo-area-messages)
                (message "Running ...")
                (shell-command cmd-str "*run-current-file output*" ))
            (message "No recognized program file suffix for this file."))))))))
(bind-to-modi-map "l" #'modi/run-current-file)

;; Help Functions +
;; http://www.emacswiki.org/emacs/HelpPlus
(use-package help-fns+
  :load-path "elisp/manually-synced/help-fns-plus"
  :defer 20
  :config
  (progn
    (bind-keys
     :map help-map
     ("c" . describe-command) ;Overrides `describe-key-briefly'
     ("C-c" . describe-char)  ;Overrides `describe-command'
     ("C-k" . describe-key-briefly)
     ("C-i" . hydra-info-to/body) ;this binding is displayed as 'TAB' in the mini-buffer
     ("A" . apropos)
     ("H" . Info-history)
     ("M-a" . counsel-ag-emacs-info)
     ("M-d" . Info-directory))
    (>=e "25.0"
        (bind-keys
         :map help-map
         ("o" . describe-symbol)))))

(>=e "25.0"
    (use-package saveplace
      :config
      (progn
        (save-place-mode 1))))

(use-package browse-url
  :commands (browse-url-chrome) ;https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31828
  :config
  (progn
    (setq browse-url-browser-function
          (cond
           ((executable-find "firefox")
            #'browse-url-firefox)
           (t
            #'browse-url-default-browser)))))

;; Unset keys
(global-unset-key (kbd "C-z"))          ;Bound to `suspend-frame' by default
;; `suspend-frame' can be called using C-x C-z too.
;; C-z is used as prefix key by me in tmux. So removing the C-z binding from
;; emacs makes it possible to use emacs in -nw (no-window) mode in tmux without
;; any key binding contention.

;; http://endlessparentheses.com/sweet-new-features-in-24-4.html
;; Hook `eval-expression-minibuffer-setup-hook' is run by `eval-expression'
;; on entering the minibuffer.
;; Below enables ElDoc inside the `eval-expression' minibuffer.
;; Call `M-:' and type something like `(message.' to see what ElDoc does :)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; https://github.com/kaushalmodi/.emacs.d/issues/7
(defun modi/startup-time()
  "Print the time it takes to load the emacs config."
  (message (format "init.el loaded in %s." (emacs-init-time))))
(add-hook 'emacs-startup-hook #'modi/startup-time)

;; Don't put the build system info "built on <IP>" in emacs bug reports
(defun modi/advice-report-emacs-bug-without-build-system (orig-fun &rest args)
  "Do not insert the user system IP when creating emacs bug report."
  (let (emacs-build-system)         ;Temporarily set `emacs-build-system' to nil
    (apply orig-fun args)))
(advice-add 'report-emacs-bug :around #'modi/advice-report-emacs-bug-without-build-system)

;; Recursively list files in a given directory
;; http://turingmachine.org/bl/2013-05-29-recursively-listing-directories-in-elisp.html
(defun directory-files-recursive (directory maxdepth full match ignore)
  "List files in DIRECTORY and in its sub-directories.
If FULL is non-nil, return absolute file names.  Otherwise return names
that are relative to the specified directory.
Return files that match the regular expression MATCH but ignore
files and directories that match IGNORE (IGNORE is tested before MATCH. Recurse only
to depth MAXDEPTH. If zero or negative, then do not recursion."
  (let* ((files-list '())
         (current-directory-list (directory-files directory full)))
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond
         ((and
           (not (string= "" ignore))
           (string-match ignore f))
          nil)
         ((and (file-regular-p f)
               (file-readable-p f)
               (string-match match f))
          (setq files-list (cons f files-list)))
         ((and (file-directory-p f)
               (file-readable-p f)
               (not (string-equal "." (substring f -1)))
               (not (string-equal ".." (substring f -2)))
               (> maxdepth 0))
          (setq files-list
                (append files-list
                        (directory-files-recursive f (1- maxdepth) full match ignore))))))
      (setq current-directory-list (cdr current-directory-list)))
    files-list))

;; Convert list to string
(defun convert-list-to-string (list delimiter)
  "Return the input LIST as a string with LIST elements separated by DELIMITER."
  (let ((str ""))
    (while list
      (let ((i (car list)))
        (if (string= "" str)
            (setq str (concat str (message "%s" i)))
          (setq str (concat str delimiter (message "%s" i)))))
      (setq list (cdr list)))
    str))

(use-package calendar
  :defer t
  :config
  (progn
    (add-hook 'calendar-today-visible-hook #'calendar-mark-today))) ;Highlight today's date

(use-package seconds-to-human-time
  :load-path "elisp/misc"
  :commands (seconds-to-human-time))

(use-package fontawesome-choose
  :load-path "elisp/misc"
  :commands (fontawesome-choose))

;; http://github.com/larstvei/try
(use-package try
  :commands (try))

;; electric-pair-mode
(use-package elec-pair
  :config
  (progn
    ;; Global enable Electric Pair mode
    (electric-pair-mode t)
    (defalias 'ep 'electric-pair-local-mode)))

(use-package esh-mode
  :bind
  (:map eshell-mode-map
   ("C-j" . eshell-send-input)))

(use-package re-builder
  :bind
  (:map reb-mode-map
   ("M-?" . hydra-reb/body)
   ("C-c C-p" . reb-prev-match)
   ("C-c C-n" . reb-next-match)
   ("M-p" . reb-prev-match)
   ("M-n" . reb-next-match))
  :config
  (progn
    (defhydra hydra-reb (:color teal :hint nil)
      "
re-builder (map prefix: C-c):
  _C-b_: change-target-buffer    _C-p_: prev-match (or just M-p)
  _C-c_: toggle-case             _C-n_: next-match (or just C-n)
  _C-e_: enter-subexp-mode       _C-u_: force-update
  _TAB_: change-syntax           _C-w_: copy
  _C-q_: reb-quit
"
      ("C-b" reb-change-target-buffer)
      ("C-c" reb-toggle-case)
      ("C-e" reb-enter-subexp-mode)
      ("TAB" reb-change-syntax)
      ("C-q" reb-quit)
      ("C-p" reb-prev-match)
      ("C-n" reb-next-match)
      ("C-u" reb-force-update)
      ("C-w" reb-copy)
      ("q" nil "cancel")
      ("C-g" nil "cancel"))))

;; rmsbolt - a compiler output viewer(https://godbolt.org/)
;; http://gitlab.com/jgkamat/rmsbolt
(use-package rmsbolt
  :defer t)

;; https://www.emacswiki.org/emacs/AsciiTable
;; Characters in the coded character set unicode: M-x `list-charset-chars' RET unicode-bmp RET
(defun ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (let ((buffer-name "*ascii*"))
    (switch-to-buffer buffer-name)
    (erase-buffer)
    (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
    (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
    (setq lower32 '("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
  		            "bs" "ht" "nl" "vt" "np" "cr" "so" "si"
  		            "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
  		            "can" "em" "sub" "esc" "fs" "gs" "rs" "us"
  		            ))
    (save-excursion (let ((i -1))
                      (insert "ASCII characters 0 thru 127.\n\n")
                      (insert " Hex  Oct  Dec  Char       |  Hex  Oct  Dec  Char|  Hex  Oct  Dec  Char|  Hex  Oct  Dec  Char\n")
                      (insert "---------------------------+---------------------+---------------------+---------------------\n")
                      (while (< i 31)
                        (insert (format "%4x %4o %4d %4s (%4s) | %4x %4o %4d %4s | %4x %4o %4d %4s | %4x %4o %4d %4s\n"
                                        (setq i (+ 1  i)) i i (single-key-description i) (elt lower32 i)
                                        (setq i (+ 32 i)) i i (single-key-description i)
                                        (setq i (+ 32 i)) i i (single-key-description i)
                                        (setq i (+ 32 i)) i i (single-key-description i)))
                        (setq i (- i 96)))))
    (with-current-buffer buffer-name
      (help-mode))))

;; https://stackoverflow.com/questions/12003231/how-do-i-convert-a-string-of-hex-into-ascii-using-elisp
;; ASCII-HEX converion
(defun my/hex-decode-string (hex-string)
  (let ((res nil))
    (dotimes (i (/ (length hex-string) 2) (apply #'concat (reverse res)))
      (let ((hex-byte (substring hex-string (* 2 i) (* 2 (+ i 1)))))
        (push (format "%c" (string-to-number hex-byte 16)) res)))))

(defun my/hex-encode-string (ascii-string)
  (let ((res nil))
    (dotimes (i (length ascii-string) (apply #'concat (reverse res)))
      (let ((ascii-char (substring ascii-string i  (+ i 1))))
        (push (format "%x" (string-to-char ascii-char)) res)))))

(defun my/hex-decode-region (start end)
  "Decode a hex string in the selected region."
  (interactive "r")
  (save-excursion
    (let* ((decoded-text
            (my/hex-decode-string
             (buffer-substring start end))))
      (delete-region start end)
      (insert decoded-text))))

(defun my/hex-encode-region (start end)
  "Encode a hex string in the selected region."
  (interactive "r")
  (save-excursion
    (let* ((encoded-text
            (my/hex-encode-string
             (buffer-substring start end))))
      (delete-region start end)
      (insert encoded-text))))

;; Tue Jul 30 13:49:35 EDT 2019 - kmodi
;; Commenting out the below as I do not think they are needed.
;; Or rather, I need to better understand if and how these settings
;; are needed.
;; (use-package auth-source
;;   :preface
;;   (progn
;;     ;; https://magit.vc/manual/ghub/How-Ghub-uses-Auth_002dSource.html
;;     ;; The variable auth-sources controls how and where Auth-Source
;;     ;; stores new secrets and where it looks for known secrets. The
;;     ;; default value is ("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"),
;;     ;; which means that it looks in all of these files in order to
;;     ;; find secrets and that it stores new secrets in ~/.authinfo
;;     ;; because that is the first element of the list. It doesn’t
;;     ;; matter which files already do or don’t exist when storing a new
;;     ;; secret, the first file is always used.

;;     ;; Secrets are stored in ~/.authinfo in plain text. If you don’t
;;     ;; want that (good choice), then you have to customize
;;     ;; auth-sources, e.g. by flipping the positions of the first two
;;     ;; elements.
;;     (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))
;;   :defer t)

;; (use-package epa-file
;;   :init
;;   (progn
;;     ;; Somehow this `custom-set-variables' form is needed, else I get
;;     ;; the error "epg-context--make: GPG error: "no usable
;;     ;; configuration", OpenPGP" when trying to open any .gpg file.
;;     ;; https://stackoverflow.com/a/41767733/1219634
;;     (custom-set-variables '(epg-gpg-program  "gpg2"))
;;     (epa-file-enable)))

(bind-keys
 ;; `save-buffers-kill-terminal' kills only the current frame; it will NOT
 ;; kill the emacs server. So remap its binding to `modi/quit-emacs'.
 ([remap save-buffers-kill-terminal] . modi/quit-emacs)
 ("C-x M-c" . modi/quit-emacs-no-desktop-save))

(defun modi/restore-imp-keys ()
  "Basic `global-map' bindings saved to `modi-mode-map'.
This comes helpful in the event that the former gets wiped off due to a bug or
by mistake.
https://lists.gnu.org/archive/html/emacs-devel/2016-07/msg00519.html "
  (interactive)
  (bind-keys
   :map modi-mode-map
   ("M-w" . kill-ring-save)
   ("C-y" . yank)
   ("C-p" . previous-line)
   ("C-n" . next-line)
   ("C-b" . backward-char)
   ("C-f" . forward-char)
   ("C-c t" . hydra-toggle/body)
   ("C-h l" . view-lossage)
   ("C-x C-c" . modi/quit-emacs)
   ("C-x M-c" . modi/quit-emacs-no-desktop-save)))
(bind-key "C-c ;" #'modi/restore-imp-keys)

(bind-key "C-j" #'push-button package-menu-mode-map)


(provide 'setup-misc)

;; TIPS
;;
;; (1) Un-define a symbol/variable
;; this will make the symbol my-nasty-variable's value void
;; (makunbound 'my-nasty-variable)
;;
;; (2) Un-define a function
;; this will make the symbol my-nasty-function's
;; function definition void
;; (fmakunbound 'my-nasty-function)
;;
;; (3) See a variable value
;; `C-h v`, enter the variable name, Enter
;; Example: `C-h v tooltip-mode`
;;
;; (4) Killing buffers from an emacsclient frame
;; `C-x #`   Kills the buffer in emacsclient frame without killing the frame
;; `C-x 5 0` Kills the emacsclient frame
;;
;; (5) `C-q' is bound to `quoted-insert'
;; Example: Pressing `C-q C-l' inserts the `^l' character (form feed):  
;;
;; (6) The way to figure out how to type a particular key combination or to know
;; what a particular key combination does, do help on a key `C-h k`, and type
;; the keystrokes you're interested in. What Emacs shows in the Help buffer is
;; the string you can pass to the macro 'kbd.
;;
;; (7) How to know what the current major mode is?
;; `C-h v major-mode`
;;
;; (8) Put this line at the top of an anything.gpg file to prevent it from
;; asking for the password on each save
;; -*- epa-file-encrypt-to: ("<MY_EMAIL>") -*-
;;
;; (9) Replace a string with string suffixed with incrementing numbers
;; http://www.reddit.com/r/emacs/comments/355bm0/til_after_so_long_using_emacs/cr1l6gy
;; Let's say you want to replace all the instances of "Here" with "Here1" "Here2" "Here3" ..
;;    1. M-x query-replace-regexp
;;    2. regexp: \bHere\b
;;    3. replacement: Here\,(1+ \#)
;;  \, lets you put in some elisp to evaluate, which is incredibly
;; useful. In the s-expression here, \# is a variable available to
;; query-replace-regexp that holds the number of replacements done so
;; far (i.e. before the first replacement it's 0, before the second
;; replacement it's 1, and so on). To start the numbering at one, we
;; just add one to it with the function 1+.
;;
;; (10) C-x =     <-- `what-cursor-position' (default binding)
;;      C-u C-x = <-- `describe-char'
;;
;; (11) Package for csv navigation: csv-nav: https://github.com/emacsmirror/csv-nav
