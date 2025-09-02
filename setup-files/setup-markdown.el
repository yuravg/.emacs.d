;; Time-stamp: <2018-02-02 10:49:18 kmodi>

;; Markdown Mode
;; https://github.com/jrblevin/markdown-mode
;; http://jblevins.org/projects/markdown-mode

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("[Rr]eadme.txt\\'" . markdown-mode)
         ("README.txt\\'" . markdown-mode)
         ("README.TXT\\'" . markdown-mode))
  :config
  (progn
    ;; http://daringfireball.net/projects/markdown/
    ;; Download the Markdown source from above, extract the .pl from that
    ;; and place it in one of the folders in the environment PATH
    (when (executable-find "Markdown.pl")
      (setq markdown-command "Markdown.pl"))

    ;; https://github.com/cadadr/emacs.d
    (defun gk-markdown-preview-buffer ()
      (interactive)
      (require 'shr)
      (let* ((buf-this (buffer-name (current-buffer)))
             (buf-html (get-buffer-create
                        (format "*md-html (%s)*" buf-this))))
        (markdown-other-window (buffer-name buf-html))
        (shr-render-buffer buf-html)
        (eww-mode)
        (kill-buffer buf-html)))

    ;; Seamless editing of Markdown tables (allowed in GFM) using `orgtbl-mode'
    ;; http://stackoverflow.com/a/20912535/1219634
    ;; https://gist.github.com/yryozo/5807243
    (defun orgtbl-to-gfm (table params)
      "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
      (let* ((alignment (mapconcat (lambda (x)
                                     (if x
                                         "|--:"
                                       "|---"))
                                   org-table-last-alignment ""))
             (params2 (list :splice t
                            :hline (concat alignment "|")
                            :lstart "| " :lend " |" :sep " | ")))
        (orgtbl-to-generic table (org-combine-plists params2 params))))
    (add-hook 'markdown-mode-hook #'orgtbl-mode)

    ;; Set to a non-nil value to use asymmetric header styling
    (setq markdown-asymmetric-header t)

    (defun yura/markdown-set-indentation()
      "Customize the indentation for `markdown-mode'."
      (setq tab-width 2
            indent-tabs-mode nil))
    (add-hook 'markdown-mode-hook #'yura/markdown-set-indentation)

    ;; https://github.com/seagle0128/grip-mode
    ;; FIXME: markdown-mode-command-map not available
    (use-package grip-mode
      :ensure t
      :bind (:map markdown-mode-command-map
             ("g" . grip-mode))
      :config
      ;; Update the grip review after every text change when non-nil.
      ;; When nil, only update the preview on file save.
      (setq grip-update-after-change nil))

    (use-package pandoc
      :ensure t
      :if (executable-find "pandoc")
      ;; :hook ((markdown-mode . pandoc-mode)
      ;;        (text-mode . pandoc-mode)
      ;;        (org-mode . pandoc-mode))
      :config
      (progn
        (defun my/markdown-to-org-current-buffer ()
          "Convert current markdown buffer to org-mode format."
          (interactive)
          (if (derived-mode-p 'markdown-mode)
              (let* ((input-file (buffer-file-name))
                     (output-file (concat (file-name-sans-extension input-file) ".org")))
                (shell-command (format "pandoc -f markdown -t org '%s' -o '%s'"
                                       input-file output-file))
                (message "Converted to %s" output-file)
                (find-file output-file))
            (message "Current buffer is not in markdown-mode")))

        (defun my/markdown-to-org-new-buffer ()
          "Convert current markdown buffer to org format in a new buffer."
          (interactive)
          (if (derived-mode-p 'markdown-mode)
              (let ((markdown-content (buffer-string))
                    (new-buffer-name (concat "*org-from-" (buffer-name) "*")))
                (with-temp-buffer
                  (insert markdown-content)
                  (let ((org-content (shell-command-to-string
                                      "pandoc -f markdown -t org")))
                    (with-current-buffer (get-buffer-create new-buffer-name)
                      (erase-buffer)
                      (insert org-content)
                      (org-mode)
                      (switch-to-buffer-other-window (current-buffer))))))
            (message "Current buffer is not in markdown-mode")))

        :init
        (unless (executable-find "pandoc")
          (message "pandoc command not found in system PATH"))))

    (bind-keys
     :map markdown-mode-map
     ;; Mimicking the org-export style bindings
     ("C-c C-e o" . gk-markdown-preview-buffer)
     ("C-c C-e t". orgtbl-send-table)
     ("M-p" . markdown-previous-visible-heading)
     ("M-n" . markdown-next-visible-heading))))


(provide 'setup-markdown)

;; C-c C-s C-p - `markdown-pre-region'
;;                Indent the selected region 4 spaces to the right
;;                (code block formatting used on reddit, stackexchange, etc.)

;; Example orgtbl template:
;;
;; <!--- BEGIN RECEIVE ORGTBL foo-tbl -->
;; | a | b |
;; |---|---|
;; | c | d |
;; <!--- END RECEIVE ORGTBL foo-tbl -->
;; <!---
;;  - Title row is needed.
;;  - Horizontal rule below title row is needed.
;;  - The table identifier 'foo-tbl' after SEND has to match with that in the
;;    BEGIN RECEIVE and END RECEIVE lines above.
;; #+orgtbl: SEND foo-tbl orgtbl-to-gfm
;; | a | b |
;; |---+---|
;; | c | d |
;; -->
;;
;; 1. Paste the above template in a `markdown-mode' buffer (without the elisp
;;    comment delimiters ";;").
;; 2. Rename 'foo-tbl' to whatever is more appropriate (optional).
;; 3. With point *inside* the 'SEND' table, call `orgtbl-send-table'.
;;
;;    Above is tested to work with `hugo' (which uses the BlackFriday markdown
;; parser).
