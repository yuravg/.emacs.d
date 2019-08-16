;; Time-stamp: <2017-06-08 08:17:02 kmodi>

;; Search / Replace

;; Contents:
;;
;;  Isearch
;;  anzu
;;  Visual Regular Expression search/replace
;;  Query exchange
;;  Swiper
;;  grep
;;  Apropos
;;  Key bindings

(setq-default case-fold-search t)       ; Ignore case when searching

;;; Isearch
;; Fuzzy isearch
;; https://www.reddit.com/r/emacs/comments/3yxk2x/flexible_isearch_without_a_package
;; Isearch convenience, space matches anything (non-greedy)
(setq search-whitespace-regexp ".*?")

;; Allow scrolling while isearch is active
;; Example: C-s foo C-l (to recenter the point in buffer to center/top/bottom)
;; http://emacs.stackexchange.com/a/10313/115
(setq isearch-allow-scroll t)
;; The beauty of scrolling while searching is that the current match never goes
;; off-screen. So you can even use C-v/M-v without worrying that you'll lose
;; the current match location.

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-isearch.el
;; DEL during isearch should edit the search string, not jump back to
;; the previous result
(define-key isearch-mode-map [remap isearch-delete-char] #'isearch-del-char)

(defun modi/isearch-backward-symbol-at-point ()
  "Do incremental search backward for a symbol found near point.
Like ordinary incremental search except that the symbol found at point
is added to the search string initially as a regexp surrounded
by symbol boundary constructs \\_< and \\_>.
See the command `isearch-forward-symbol' for more information."
  (interactive)
  (isearch-mode (not :forward) nil nil nil 'isearch-symbol-regexp)
  (let ((bounds (find-tag-default-bounds)))
    (cond
     (bounds
      (when (< (car bounds) (point))
        (goto-char (car bounds)))
      (isearch-yank-string
       (buffer-substring-no-properties (car bounds) (cdr bounds))))
     (t
      (setq isearch-error "No symbol at point")
      (isearch-update)))))

(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)

;;; anzu
;; https://github.com/syohex/emacs-anzu
(use-package anzu
  :bind (:map modi-mode-map
         ("C-c r" . anzu-query-replace)
         ("C-c C-r" . anzu-query-replace-regexp))
  :config
  (progn
    (setq anzu-mode-lighter "")
    (setq anzu-search-threshold 1000)
    (setq anzu-replace-to-string-separator " => ")

    (global-anzu-mode 1)))

;;; Visual Regular Expression search/replace
(use-package visual-regexp
  :bind (:map modi-mode-map
         ("C-c q" . vr/query-replace)
         ("C-c M" . vr/mc-mark))
  :config
  (progn
    (setq vr/default-feedback-limit 300)))

;;; Query exchange
;; Inspired from http://www.emacswiki.org/emacs/QueryExchange and definition of
;; `query-replace-regexp' from replace.el
(defun query-exchange (string-1 string-2 &optional delimited start end)
  "Exchange string-1 and string-2 interactively.

The user is prompted at each instance like query-replace. Exchanging
happens within a region if one is selected."
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Query replace"
                   (if current-prefix-arg " word" "")
                   " regexp"
                   (if (and transient-mark-mode mark-active) " in region" ""))
           t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           ;; These are done separately here
           ;; so that command-history will record these expressions
           ;; rather than the values they had this time.
           (if (and transient-mark-mode mark-active)
               (region-beginning))
           (if (and transient-mark-mode mark-active)
               (region-end)))))
  (perform-replace
   (concat "\\(" string-1 "\\)\\|" string-2)
   '(replace-eval-replacement replace-quote
                              (if (match-string 1) string-2 string-1))
   t t delimited nil nil start end))

;;; Swiper
;; https://github.com/abo-abo/swiper
(use-package swiper
  :bind (:map isearch-mode-map
         ("M-i" . swiper-from-isearch)) ; isearch > swiper
  :bind (:map modi-mode-map
         ("C-M-i" . modi/swiper))
  :bind (:map swiper-map
         ("C-h" . backward-delete-char))
  :config
  (progn
    (defun modi/swiper (arg)
      "Start swiper with input as the selected region.

If a region is not selected and,
  - If ARG is nil, start swiper with the symbol at point as input.
  - Elseswiper without any arguments (stock behavior)."
      (interactive "P")
      (if (use-region-p)
          (let ((b (region-beginning))
                (e (region-end)))
            (deactivate-mark)
            (swiper (buffer-substring-no-properties b e)))
        (if arg
            (swiper) ; C-u
          (swiper (modi/get-selected-text-or-symbol-at-point)))))


    (bind-key "M-a" #'swiper-avy swiper-map))) ; swiper > avy

;;; grep
;; Search for the highlighted string in ALL buffers `offby1/search-all-buffers'
;; http://stackoverflow.com/a/2642655/1219634
(use-package grep
  :commands (offby1/search-all-buffers) ; need to require `grep' for `grep-read-regexp'
  :config
  (progn
    (defcustom offby1/search-all-buffers-ignored-files
      (list (rx-to-string
             '(and bos
                   (or ".bash_history" "TAGS")
                   eos)))
      "Files to ignore when searching buffers via \\[offby1/search-all-buffers]."
      :type 'editable-list)

    ;; http://stackoverflow.com/q/7014455/1219634
    ;; Workaround for the issue where the `grep-find-template' stays nil when
    ;; `rgrep' is called non-interactively.
    (grep-compute-defaults)

    (defun offby1/search-all-buffers (regexp prefix)
      "Searches file-visiting buffers for occurence of REGEXP.  With
prefix > 1 (i.e., if you type C-u \\[offby1/search-all-buffers]),
searches all buffers."
      (interactive (list (grep-read-regexp)
                         current-prefix-arg))
      (message "Regexp is %s; prefix is %s" regexp prefix)
      (multi-occur
       (if (member prefix '(4 (4)))
           (buffer-list)
         (remove-if
          (lambda (b)
            (some
             (lambda (rx)
               (string-match rx (file-name-nondirectory
                                 (buffer-file-name b))))
             offby1/search-all-buffers-ignored-files))
          (remove-if-not 'buffer-file-name (buffer-list))))
       regexp))
    (bind-to-modi-map "s" #'offby1/search-all-buffers)))

;;; Apropos
(use-package apropos
  :defer t
  :bind (:map apropos-mode-map
         ("C-j" . push-button))
  :config
  (progn
    (setq apropos-do-all t)))     ;Make apropos commands search more extensively

;;; Key bindings
(bind-keys
 :map modi-mode-map
 ("C-S-s" . isearch-forward-symbol-at-point)
 ("C-S-r" . modi/isearch-backward-symbol-at-point))

(bind-keys
 ;; :commands (unbind-key "C-j" isearch-mode-map) ; default binding: `isearch-printing-char'
 :map isearch-mode-map
 ("C-j" . isearch-exit)
 ("C-h" . isearch-del-char))


(provide 'setup-search)
