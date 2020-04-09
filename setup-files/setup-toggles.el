;; Time-stamp: <2017-05-11 13:40:41 kmodi>

;; Toggles

(defhydra hydra-toggle (:color blue
                        :columns 7)
  "Toggle"
  ("aa"    artist-mode "artist mode")
  ("af"    auto-fill-mode "auto fill")
  ("ar"    auto-revert-mode "auto revert")
  ("aw"    adaptive-wrap-prefix-mode "adaptive wrap")
  ("b"     duplicate-current-window "double window(C-u: del other)") ; C-u available for `duplicate-current-window'
  ("d"     toggle-debug-on-error "debug on error")
  ("D"     modi/toggle-debug "debug on entry")
  ("e"     modi/toggle-edebug "edebug")
  ("f"     toggle-fill-unfill "fill/unfill")
  ("F"     follow-mode "follow mode")
  ("g"     (lambda (arg) (interactive "P")
             ;; turn on only one mode: `highlight-indent-guides-mode' or `indent-guide-mode'
             (if arg
                 (let ((current-prefix-arg nil))
                   (call-interactively 'indent-guide-mode)
                   (highlight-indent-guides-mode -1))
               (progn
                 (call-interactively 'highlight-indent-guides-mode)
                 (indent-guide-mode -1)))) "indent guide(C-u: near point)")
  ("hc"    hardcore-mode "arrow key navigation")
  ("hl"    hl-line-mode "highlight line")
  ("hs"    modi/hideshow-mode "hideshow mode")
  ("i"     modi/imenu-list-display-toggle "imenu list")
  ("k"     key-chord-mode "key chord mode" :color red)
  ("l"     (lambda (arg) (interactive "P")
             ;; turn on only one mode: `linum-mode' or `nlinum-mode'
             (if arg
                 (let ((current-prefix-arg nil))
                   (call-interactively #'nlinum-mode)
                   (linum-mode -1))
               (progn
                 (call-interactively #'linum-mode)
                 (nlinum-mode -1))))
   "line numbers(C-u: jit-lock)")
  ("m"     modi-mode "Modi mode")
  ("n"     neotree-toggle "neotree")
  ("o"     modi/toggle-one-window "one window" :color red)
  ("p"     prez-mode "presentation mode")
  ("r"     yura/revert-buffer-no-confirm "revert buffer(C-u: & modes)") ; C-u available for `yura/revert-buffer-no-confirm'
  ("sa"    scroll-all-mode "scroll all mode")
  ("t"     (lambda (arg) (interactive "P")
             (if arg (google-translate-at-point-reverse) (google-translate-at-point))) "translate(C-u: reverse)")
  ("M-t"     (lambda (arg) (interactive "P")
               (if arg (google-translate-query-translate-reverse) (google-translate-query-translate))) "translate query(C-u: reverse)")
  ("C-t"   toggle-truncate-lines "truncate lines" :color red)
  ("C-r"   dired-toggle-read-only "read only/editable") ; generalized `read-only-mode'
  ("v"     view-mode "view mode") ; make buffer read-only, then use SPC/DEL to nav
  ("V"     visible-mode "visible mode") ; view all hidden text in org-mode (e.g. links)
  ("w"     yura/whitespace-with-newline "whitespace mode")
  ("q"     nil "cancel" :color blue))

(bind-key "s-t" #'hydra-toggle/body)
(bind-key "C-c t" #'hydra-toggle/body)


(provide 'setup-toggles)

;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html
