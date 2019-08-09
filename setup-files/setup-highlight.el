;; Time-stamp: <2019-03-22 17:05:02 kmodi>

;; Highlight stuff

;; Contents:
;;
;;  Hi-lock
;;  Highlight Global
;;  Volatile Highlights
;;  Auto Highlight Symbol
;;  Highlight line
;;  Idle Highlight
;;  Highlight
;;    Notes
;;  Highlight Anything
;;  Colors for highlight
;;  Notes

;;; Hi-lock
(use-package hi-lock
  :config
  (progn
    ;; Patch the `hi-lock-face-buffer' aka `highlight-regexp' to pick the
    ;; selected region to derive a regexp if a region is active.
    (defun hi-lock-face-buffer (regexp &optional face)
      "Set face of each match of REGEXP to FACE.
Interactively, prompt for REGEXP using `read-regexp', then FACE.
Use the global history list for FACE.

Use Font lock mode, if enabled, to highlight REGEXP.  Otherwise,
use overlays for highlighting.  If overlays are used, the
highlighting will not update as you type."
      (interactive
       (list
        (hi-lock-regexp-okay
         (read-regexp "Regexp to highlight"
                      (if (use-region-p)
                          ;; Use `rx' to generate regexp for selected text.
                          ;; Example: regexp to find "a.b" text would be
                          ;; "a\.b"
                          (let ((str (buffer-substring-no-properties
                                      (region-beginning) (region-end))))
                            (eval `(rx ,str)))
                        'regexp-history-last)))
        (hi-lock-read-face-name)))
      (or (facep face) (setq face 'hi-yellow))
      (unless hi-lock-mode (hi-lock-mode 1))
      (hi-lock-set-pattern regexp face))

    ;; Don't scan the file beyond 1000 characters to look for the Hi-Lock patterns.
    (setq hi-lock-file-patterns-range 1000)

    ;; Don't ask before highlighting any Hi-Lock: pattern found in a file
    ;; Below, (lambda (pattern) t) simply always returns `t' regardless of
    ;; what the `pattern' input is.
    (setq hi-lock-file-patterns-policy (lambda (pattern) t))

    ;; Mark the `hi-lock-file-patterns' variable as safe so that it can be
    ;; set in `.dir-locals.el' files.
    (put 'hi-lock-file-patterns 'safe-local-variable 'identity)

    ;; highlighting commands prompt for the face to use.
    (setq hi-lock-auto-select-face nil)

    (defun modi/hi-lock-face-symbol-at-point-or-sel ()
      "If a region is selected, highlight each instance of that.
Else highlight each instance of the symbol at point.

Uses the next face from `hi-lock-face-defaults' without prompting,
unless you use a prefix argument. Uses `find-tag-default-as-symbol-regexp' to
retrieve the symbol at point.

This uses Font lock mode if it is enabled; otherwise it uses overlays,
in which case the highlighting will not update as you type."
      (interactive)
      (let* ((regexp (hi-lock-regexp-okay
                      (cond ((use-region-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end)))
                            (t
                             (find-tag-default-as-symbol-regexp)))))
             (hi-lock-auto-select-face t)
             (face (hi-lock-read-face-name)))
        (or (facep face) (setq face 'hi-yellow))
        (unless hi-lock-mode (hi-lock-mode 1))
        (hi-lock-set-pattern regexp face)))

    ;; Enable `hi-lock-mode' in `text-mode' too
    ;; The hi-lock fontification will not be visible (the `font-lock-keywords'
    ;; variable will not be updated unless `font-lock-fontified' is already `t'.
    ;; This was derived by studying the definition of `hi-lock-font-lock-hook'
    ;; function.
    (defun modi/hi-lock-enable-in-text-mode ()
      (setq-local font-lock-fontified t))
    (add-hook 'text-mode-hook #'modi/hi-lock-enable-in-text-mode)

    (defun modi/unhighlight-all-in-buffer ()
      "Remove all highlights made by `hi-lock' from the current buffer.
The same result can also be be achieved by \\[universal-argument] \\[unhighlight-regexp]."
      (interactive)
      ;; `unhighlight-regexp' is aliased to `hi-lock-unface-buffer'
      (hi-lock-unface-buffer t))
    (bind-key "h U" #'modi/unhighlight-all-in-buffer search-map)

    (global-hi-lock-mode 1)

    ;; Unbind the "C-x w" bindings because "M-s h" bindings provide the same thing.
    (bind-key "C-x w" nil hi-lock-map)

    (defalias 'lh 'hi-lock-face-buffer)
    (defalias 'lu 'hi-lock-unface-buffer)
    (defalias 'lhl 'highlight-lines-matching-regexp)

    (bind-keys
     :map modi-mode-map
     ("C-." . modi/hi-lock-face-symbol-at-point-or-sel))))

;;; Highlight Global
;; https://github.com/glen-dai/highlight-global
(use-package highlight-global
  :load-path "elisp/highlight-global"
  :commands (highlight-global-hl-frame-toggle
             highlight-global-clear-hl-frame)
  :init
  (progn
    (bind-to-modi-map "h" #'highlight-global-hl-frame-toggle)
    (bind-to-modi-map "H" #'highlight-global-clear-hl-frame)))

;;; Volatile Highlights
;; https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :config
  (progn
    (volatile-highlights-mode 1)))

;;; Auto Highlight Symbol
;; https://github.com/emacsmirror/auto-highlight-symbol
(use-package auto-highlight-symbol
  :bind (:map modi-mode-map
         ("C-*"             . auto-highlight-symbol-mode)
         ("<C-kp-multiply>" . auto-highlight-symbol-mode))
  :config
  (progn
    (setq ahs-default-range 'ahs-range-whole-buffer)

    (bind-keys
     :map auto-highlight-symbol-mode-map
     ("M-<"     . ahs-backward)
     ("M->"     . ahs-forward)
     ("M--"     . ahs-back-to-start)
     ("C-x C-'" . ahs-change-range)
     ("C-x C-a" . ahs-edit-mode))))

;;; Highlight line
(use-package hl-line
  :config
  (progn
    ;; Highlight the line only in the active window
    (setq hl-line-sticky-flag nil)

    ;; hl-line+
    ;; http://www.emacswiki.org/emacs/hl-line+.el
    (use-package hl-line+
      :load-path "elisp/manually-synced/hl-line-plus"
      :config
      (progn
        (toggle-hl-line-when-idle 1) ; Highlight line only when idle
        ;; Number of seconds of idle time after when the line should be highlighted
        (setq hl-line-idle-interval 5)
        ;; Number of seconds for `hl-line-flash' to highlight the line
        (setq hl-line-flash-show-period 3)))))

;;; Idle Highlight
;; http://www.emacswiki.org/emacs/IdleHighlight
;; https://github.com/nonsequitur/idle-highlight-mode
(use-package idle-highlight-mode
  :load-path "elisp/manually-synced/idle-highlight-mode"
  :config
  (progn
    ;; https://github.com/nonsequitur/idle-highlight-mode/blob/master/idle-highlight-mode.el
    (defun yura/idle-highlight-mode-enable ()
      "Turn on `idle-highlight-mode' for each buffer separately."
      (make-local-variable 'column-number-mode)
      (column-number-mode t)
      (if window-system (hl-line-mode t))
      (idle-highlight-mode t))

    (defconst yura/idle-highlight-mode-hooks '(emacs-lisp-mode-hook
                                               tcl-mode-hook
                                               java-mode-hook
                                               python-mode-hook
                                               verilog-mode-hook
                                               c-mode-hook
                                               c++-mode-hook
                                               makefile-mode-hook
                                               makefile-gmake-mode-hook
                                               sh-mode-hook
                                               text-mode-hook
                                               bat-mode-hook
                                               conf-space-mode-hook)
      "List of hooks of major modes in which `idle-highlight-mode' should be enabled.")

    (dolist (hook yura/idle-highlight-mode-hooks)
      (add-hook hook #'yura/idle-highlight-mode-enable))))

;;; Highlight
(use-package highlight
  :load-path "elisp/manually-synced/highlight"
  :commands (turn-on-highlighting-commands highlight)
  :config
  (progn
    (defun turn-on-highlighting-commands()
      "Turn on Highlighting commands of `highlight'."
      (interactive)
      (highlight))))

;;;; Notes
;; Prefix key 'prefix': `C-x X'
;; Prefix to highlight/unhighlight: prefix h/u
;;
;; (1) Wholly highlight the current buffer:
;; 1. Set face(if you want to change the default face - 'highlight'):
;;    prefix r -- `hlt-replace-highlight-face'
;; 2. Highlight: prefix h r RET -- `hlt-highlight-regions'
;; 3. Unhighlight: prefix u r RET -- `hlt-unhighlight-region'

;;; Highlight Anything
;; https://github.com/boyw165/hl-anything
(use-package hl-anything
  :if (not (bound-and-true-p disable-pkg-hl-anything))
  ;; This package has known to cause issues with the `list-colors-display'
  ;; command. The buffer that opens on calling that command does not show the
  ;; colors. The issue is fixed temporarily by uncommenting the below line
  ;; and restarting emacs - https://github.com/boyw165/hl-anything/issues/14
  ;; Also causes to show this error at startup:
  ;;   org-mode fontification error
  :init
  (progn
    (setq hl-highlight-save-file (locate-user-emacs-file "hl-save")))
  :config
  (progn
    (hl-highlight-mode 1)

    (defun my/hl-anything (local)
      "Highlight the thing at point globally in all buffers.

If LOCAL is non-nil, highlight only in the current buffer."
      (interactive "P")
      (if local
          (hl-highlight-thingatpt-local)
        (hl-highlight-thingatpt-global)))

    (defun my/unhl-anything (local)
      "Un-highlight the thing at point globally in all buffers.

If LOCAL is non-nil, un-highlight only in the current buffer."
      (interactive "P")
      (if local
          (hl-unhighlight-all-local)
        (hl-unhighlight-all-global)))

    (defhydra hydra-hl-anything (:color teal
                                 :hint nil)
      "
^^^^       Highlight               ^^             Jump                         ^^  Save                     ^^    Other
^^^^-------------------------------^^-----------------------------------------^^^^--------------------------^^-----------------------
  _h_/_H_: highlight (global/local)          _n_: next hightlight             _s_: save highlights           _t_: toggle highlights
  _u_/_U_: un-highlight (global/local)       _p_: previous highlight          _r_: restore highlights
  _f_/_d_: lock face/unface (regexp)
  _l_/_d_: highlight/unface line (regexp)
"
      ("h" my/hl-anything)
      ("H" (my/hl-anything :local))
      ("u" my/unhl-anything)
      ("U" (my/unhl-anything :local))
      ("n" hl-find-next-thing :color red)
      ("p" hl-find-prev-thing :color red)
      ("s" hl-save-highlights)
      ("r" hl-restore-highlights)
      ("t" hl-global-highlight-on/off :color red)
      ("f" hi-lock-face-buffer)
      ("d" hi-lock-unface-buffer)
      ("l" highlight-lines-matching-regexp)
      ("q" nil "cancel"))
    (bind-key "C-c h" #'hydra-hl-anything/body modi-mode-map)))

;;; Colors for highlight
(defface hi-tomato1
  '((((background dark)) (:background "tomato1" :foreground "black"))
    (t (:background "tomato1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-red
  '((((background dark)) (:background "red" :foreground "black"))
    (t (:background "red")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-yellow
  '((((background dark)) (:background "yellow" :foreground "black"))
    (t (:background "yellow")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-pink
  '((((background dark)) (:background "pink" :foreground "black"))
    (t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-gold
  '((((background dark)) (:background "gold" :foreground "black"))
    (t (:background "gold")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-orchid
  '((((background dark)) (:background "orchid" :foreground "black"))
    (t (:background "orchid")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-light-salmon
  '((((background dark)) (:background "light salmon" :foreground "black"))
    (t (:background "light salmon")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-magenta1
  '((((background dark)) (:background "magenta1" :foreground "black"))
    (t (:background "magenta1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-tan
  '((((background dark)) (:background "tan" :foreground "black"))
    (t (:background "tan")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-light-green
  '((((background dark)) (:background "light green" :foreground "black"))
    (t (:background "light green")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-green1
  '((((background dark)) (:background "green1" :foreground "black"))
    (t (:background "green1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-green3
  '((((background dark)) (:background "green3" :foreground "black"))
    (t (:background "green3")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-green4
  '((((background dark)) (:background "green4" :foreground "black"))
    (t (:background "green4")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-dark-green
  '((((background dark)) (:background "dark green" :foreground "black"))
    (t (:background "dark green")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-dodger-blue
  '((((background dark)) (:background "dodger blue" :foreground "black"))
    (t (:background "dodger blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-light-blue
  '((((background dark)) (:background "light blue" :foreground "black"))
    (t (:background "light blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-deepskyblue
  '((((background dark)) (:background "DeepSkyBlue" :foreground "black"))
    (t (:background "DeepSkyBlue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-lightcyan2
  '((((background dark)) (:background "LightCyan2" :foreground "black"))
    (t (:background "LightCyan2")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-sienna
  '((((background dark)) (:background "sienna3" :foreground "black"))
    (t (:background "sienna3")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-gray90
  '((((background dark)) (:background "gray90" :foreground "black"))
    (t (:background "gray90")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-gray80
  '((((background dark)) (:background "gray80" :foreground "black"))
    (t (:background "gray80")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-wheat1
  '((((background dark)) (:background "wheat1" :foreground "black"))
    (t (:background "wheat1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-cornflower-blue
  '((((background dark)) (:background "cornflower blue" :foreground "black"))
    (t (:background "cornflower blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-aquamarine
  '((((background dark)) (:background "aquamarine" :foreground "black"))
    (t (:background "aquamarine")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-cyan
  '((((background dark)) (:background "cyan" :foreground "black"))
    (t (:background "cyan")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-cyan3
  '((((background dark)) (:background "cyan3" :foreground "black"))
    (t (:background "cyan3")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-olive-drab
  '((((background dark)) (:background "olive drab" :foreground "black"))
    (t (:background "olive drab")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-chocolate
  '((((background dark)) (:background "chocolate" :foreground "black"))
    (t (:background "chocolate")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-violet
  '((((background dark)) (:background "violet" :foreground "black"))
    (t (:background "violet")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-peach-puff
  '((((background dark)) (:background "peach puff" :foreground "black"))
    (t (:background "peach puff")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-slateblue1
  '((((background dark)) (:background "SlateBlue1" :foreground "black"))
    (t (:background "SlateBlue1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-purple1
  '((((background dark)) (:background "purple1" :foreground "black"))
    (t (:background "purple1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-orange
  '((((background dark)) (:background "orange" :foreground "black"))
    (t (:background "orange")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-peru
  '((((background dark)) (:background "peru" :foreground "black"))
    (t (:background "peru")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-dark-khaki
  '((((background dark)) (:background "dark khaki" :foreground "black"))
    (t (:background "dark khaki")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(face-spec-set 'hi-black-b
               '((((background dark)) (:weight bold :foreground "black"))
                 (t (:weight bold :foreground "black")))
               'face-defface-spec)

(face-spec-set 'hi-blue-b
               '((((background dark)) (:weight bold :foreground "blue1"))
                 (t (:weight bold :foreground "blue1")))
               'face-defface-spec)

(face-spec-set 'hi-green-b
               '((((background dark)) (:weight bold :foreground "forest green"))
                 (t (:weight bold :foreground "forest green")))
               'face-defface-spec)

(face-spec-set 'hi-red-b
               '((((background dark)) (:weight bold :foreground "red"))
                 (t (:weight bold :foreground "red")))
               'face-defface-spec)

(defface hi-orange-b
  '((((background dark)) (:weight bold :foreground "orange"))
    (t (:weight bold :foreground "orange")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-magenta-b
  '((((background dark)) (:weight bold :foreground "magenta2"))
    (t (:weight bold :foreground "magenta2")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defvar highlight-faces
  '(('hi-yellow          . 0 )
    ('hi-light-green     . 0 )
    ('hi-light-salmon    . 0 )
    ('hi-light-blue      . 0 )
    ('hi-pink            . 0 )
    ('hi-tan             . 0 )
    ('hi-cornflower-blue . 0 )
    ('hi-violet          . 0 )
    ('hi-gold            . 0 )
    ('hi-green3          . 0 )
    ('hi-peru            . 0 )
    ('hi-cyan            . 0 )
    ('hi-orange          . 0 )
    ('hi-peach-puff      . 0 )
    ('hi-green1          . 0 )
    ('hi-deepskyblue     . 0 )
    ('hi-purple1         . 0 ))
  "Default faces for hi-lock interactive functions, you could add your own.")

(setq hl-highlight-background-colors
      (quote
       ("yellow"
        "light green"
        "light salmon"
        "light blue"
        "pink"
        "tan"
        "cornflower blue"
        "violet"
        "gold"
        "green3"
        "peru"
        "cyan"
        "orange"
        "peach puff"
        "green1"
        "DeepSkyBlue"
        "purple1")))

(setq hl-highlight-foreground-colors
      (quote
       ("black"
        "black"
        "black"
        "black"
        "black"
        "black"
        "black"
        "black"
        "black"
        "black"
        "black"
        "black"
        "black"
        "black"
        "black"
        "black"
        "black")))


(provide 'setup-highlight)

;;; Notes

;; If `hi-lock-file-patterns-policy' is set to `nil' or `'never', you will need
;; to call `M-x hi-lock-find-patterns' or `M-s h f' to highlighted all
;; occurrences of Hi-Lock: patterns specified in the file.

;; The Hi-Lock regexp forms are in the form of font lock keywords. Do
;; `C-h v font-lock-keywords' to learn more.

;; Example of using Hi-lock keyword:
;; Hi-Lock: (("policy" (0 'hi-yellow prepend)))
;; Hi-Lock: end

;; Hi-Lock: (("<REGEXP>" (<SUBEXP-0> '<FACE-0> [<OVERRIDE> [<LAXMATCH>]])
;;                       (<SUBEXP-1> '<FACE-1> [<OVERRIDE> [<LAXMATCH>]])
;;                       .. ))
;; Hi-Lock: end

;; OVERRIDE and LAXMATCH are flags.
;; If OVERRIDE is t, existing fontification can be overwritten.
;;   If `keep', only parts not already fontified are highlighted.
;;   If `prepend', existing fontification is merged with the new, in
;;     which the new fontification takes precedence.
;;   If `append', existing fontification is merged with the new, in
;;     which the existing fontification takes precedence.
;; If LAXMATCH is non-nil, that means don't signal an error if there is
;; no match for SUBEXP in REGEXP.

;; Examples of Hi-Lock patterns:

;; Highlight outshine headers in `shell-script-mode':
;; # Hi-lock: (("\\(^\\s< *\\**\\)\\(\\* *.*\\)" (1 'org-hide prepend) (2 '(:inherit org-level-1 :height 1.3 :weight bold :overline t :underline t) prepend)))

;; Highlight outshine headers in `emacs-lisp-mode':
;; ;; Hi-lock: (("\\(^;\\{3,\\}\\)\\( *.*\\)" (1 'org-hide prepend) (2 '(:inherit org-level-1 :height 1.3 :weight bold :overline t :underline t) prepend)))

;; Highlight outshine headers in `verilog-mode':
;; // Hi-lock: (("\\(^// \\**\\)\\(\\* *.*\\)" (1 'org-hide prepend) (2 '(:inherit org-level-1 :height 1.3 :weight bold :overline t :underline t) prepend)))
;; If you do not want to modify the source files with the Hi-Lock meta data,
;; you can set the `hi-lock-file-patterns' variable using `.dir-locals.el' files
;; as below:
;;   (("PATH/TO/DIR"
;;     . ((verilog-mode . ((hi-lock-file-patterns
;;                          . (("\\(^// \\**\\)\\(\\* *.*\\)"
;;                              (1 'org-hide prepend)
;;                              (2 '(:inherit org-level-1
;;                                   :height 1.3
;;                                   :weight bold
;;                                   :overline t
;;                                   :underline t)
;;                                 prepend))))
;;                         ))))
;;    )
