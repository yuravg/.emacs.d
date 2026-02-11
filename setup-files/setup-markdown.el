;; Time-stamp: <2018-02-02 10:49:18 kmodi>

;; Markdown Mode
;; https://github.com/jrblevin/markdown-mode
;; http://jblevins.org/projects/markdown-mode

;; Contents:
;;
;;  markdown-mode
;;    orgtbl-to-gfm
;;    grip-mode
;;    pandoc
;;    My Customize
;;    my/markdown-cleanup-heder-list-numbers-level
;;    markdown-mode-map
;;  Notes

;;

;;; markdown-mode
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

;;;; orgtbl-to-gfm
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

;;;; grip-mode
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

;;;; pandoc
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
            (message "Current buffer is not in markdown-mode"))))

      :init
      (unless (executable-find "pandoc")
        (message "pandoc command not found in system PATH")))


;;;; My Customize
    ;; Set to a non-nil value to use asymmetric header styling
    (setq markdown-asymmetric-header t)

    (defun my/markdown-set-indentation()
      "Customize the indentation for `markdown-mode'."
      (setq tab-width 2
            indent-tabs-mode nil))
    (add-hook 'markdown-mode-hook #'my/markdown-set-indentation)

;;;; my/markdown-cleanup-heder-list-numbers-level
    (defun my/markdown-cleanup-heder-list-numbers-level ()
      "Renumber phase headers, multi-level numbered headers, and update task counters.

Processes two header patterns in Pass 1:

1. Phase headers: `# Phase N:' or `## Phase N:' etc.
   Renumbered sequentially (1, 2, 3...) in order of appearance.

2. Multi-level numbered headers: `## N.N' or `### N.N.N' etc.
   Renumbered based on the current phase and header level.

Pass 2 updates task counters (Org-mode style):

3. Task counters: `[N/M done]' suffix updated hierarchically.
   Counts DIRECT children only (exactly one level deeper).
   - `[x]' counts as done
   - `[ ]', `[~]', `[!]' count as not done
   - `[N/M done]' subtask counters: done if N=M, not done otherwise
   Headers with `[N/M done]' are processed bottom-up.

Example transformation:
  # Phase 5: intro [0/2 done]  ; becomes # Phase 1: intro [2/4 done]
  ## 5.1 task A [x]            ; becomes ## 1.1 task A [x]        (done)
  ## 5.2 task B [0/2 done]     ; becomes ## 1.2 task B [1/4 done] (not done)
  ### 5.2.1 sub [x]            ; becomes ### 1.2.1 sub [x]
  ### 5.2.2 sub [ ]            ; becomes ### 1.2.2 sub [ ]
  ### 5.2.3 sub [ ]            ; becomes ### 1.2.3 sub [ ]
  ### 5.2.4 sub [ ]            ; becomes ### 1.2.4 sub [ ]
  ## 5.3 task C [ ]            ; becomes ## 1.3 task C [ ]        (not done)
  ## 5.4 task D [0/2 done]     ; becomes ## 1.4 task D [2/2 done] (done)
  ### 5.4.1 sub [x]            ; becomes ### 1.4.1 sub [x]
  ### 5.4.2 sub [x]            ; becomes ### 1.4.2 sub [x]

Without a preceding phase header, numbering starts from 1.

Plain numbered lists (\"1. item\") are NOT modified.

See also `markdown-cleanup-list-numbers'."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (let ((phase-num 0)
              (phase-level 1)
              (counters (make-vector 7 0))
              (phase-positions '()))

          ;; Pass 1: Renumber headers and collect phase positions
          (while (not (eobp))
            (cond
             ;; Phase header: # Phase N: or ## Phase N: etc.
             ((looking-at "^\\(#+\\)\\s-+[Pp]hase\\s-+\\([0-9]+\\)\\s-*:\\(.*?\\)\\(\\s-*\\[[-0-9]+/[-0-9]+\\s-*done\\]\\)?\\s-*$")
              (setq phase-num (1+ phase-num))
              (setq phase-level (length (match-string 1)))
              (fillarray counters 0)
              (push (cons (line-number-at-pos) phase-level) phase-positions)
              (let ((num-start (match-beginning 2))
                    (num-end (match-end 2)))
                (delete-region num-start num-end)
                (goto-char num-start)
                (insert (number-to-string phase-num))))

             ;; Multi-level numbered header: ## N.N or ### N.N.N etc.
             ((looking-at "^\\(#+\\)\\s-+\\([0-9]+\\(?:\\.[0-9]+\\)+\\)\\s-")
              (let* ((level (length (match-string 1)))
                     (num-start (match-beginning 2))
                     (num-end (match-end 2))
                     (current-phase (if (zerop phase-num) 1 phase-num)))
                (aset counters level (1+ (aref counters level)))
                (cl-loop for i from (1+ level) below (length counters)
                         do (aset counters i 0))
                (let ((new-num (mapconcat #'number-to-string
                                          (cons current-phase
                                                (cl-loop for i from (1+ phase-level) to level
                                                         collect (aref counters i)))
                                          ".")))
                  (delete-region num-start num-end)
                  (goto-char num-start)
                  (insert new-num)))))
            (forward-line 1))

          ;; Pass 2: Update task counters
          (goto-char (point-min))
          (let ((headers-with-counters '()))
            ;; Collect headers with [N/M done] or Phase headers
            (while (not (eobp))
              (cond
               ((looking-at "^\\(#+\\)\\s-+[Pp]hase\\s-+[0-9]+\\s-*:")
                (push (list (line-number-at-pos)
                            (length (match-string 1))
                            'phase)
                      headers-with-counters))
               ((looking-at "^\\(#+\\)\\s-+.*\\[[0-9]+/[0-9]+\\s-*done\\]")
                (push (list (line-number-at-pos)
                            (length (match-string 1))
                            'counter)
                      headers-with-counters)))
              (forward-line 1))

            ;; Sort by level descending (process deepest first)
            (setq headers-with-counters
                  (sort headers-with-counters
                        (lambda (a b) (> (nth 1 a) (nth 1 b)))))

            ;; Update each header's counter based on direct children
            (dolist (header-info headers-with-counters)
              (let* ((header-line (nth 0 header-info))
                     (header-level (nth 1 header-info))
                     (child-level (1+ header-level))
                     (done-count 0)
                     (total-count 0))
                ;; Go to header line and scan for direct children
                (goto-char (point-min))
                (forward-line (1- header-line))
                (forward-line 1)
                ;; Count direct children until we hit same/higher level header
                ;; Skip content inside fenced code blocks (``` or ~~~)
                (let ((continue-loop t)
                      (cur-level nil)
                      (in-code-block nil))
                  (while (and continue-loop (not (eobp)))
                    (cond
                     ;; Toggle code block state on fence markers
                     ;; Use [ \t]* instead of \s-* to avoid matching across newlines
                     ((looking-at "^[ \t]*\\(```\\|~~~\\)")
                      (setq in-code-block (not in-code-block))
                      (forward-line 1))
                     ;; Skip content inside code blocks
                     (in-code-block
                      (forward-line 1))
                     ;; Check if it's a header (outside code block)
                     ((looking-at "^\\(#+\\)\\s-")
                      (setq cur-level (length (match-string 1)))
                      (cond
                       ((<= cur-level header-level)
                        (setq continue-loop nil))
                       ((= cur-level child-level)
                        (cond
                         ((looking-at "^#+\\s-+.*\\[\\([0-9]+\\)/\\([0-9]+\\)\\s-*done\\]")
                          (setq total-count (1+ total-count))
                          (when (= (string-to-number (match-string 1))
                                   (string-to-number (match-string 2)))
                            (setq done-count (1+ done-count))))
                         ((looking-at "^#+\\s-+.*\\[\\([x~! ]\\)\\]")
                          (setq total-count (1+ total-count))
                          (when (string= (match-string 1) "x")
                            (setq done-count (1+ done-count)))))
                        (forward-line 1))
                       (t (forward-line 1))))
                     (t (forward-line 1)))))

                ;; Update the header with task counter
                ;; Use delete-region + insert instead of replace-match to preserve newlines
                (when (> total-count 0)
                  (goto-char (point-min))
                  (forward-line (1- header-line))
                  (let ((counter-str (format " [%d/%d done]" done-count total-count))
                        (line-end (line-end-position)))
                    (cond
                     ((looking-at "^\\(#+\\s-+[Pp]hase\\s-+[0-9]+\\s-*:.*?\\)\\(\\s-*\\[[-0-9]+/[-0-9]+\\s-*done\\]\\)?\\s-*$")
                      (let ((new-content (concat (match-string 1) counter-str)))
                        (delete-region (point) line-end)
                        (insert new-content)))
                     ((looking-at "^\\(#+\\s-+.*?\\)\\s-*\\[[-0-9]+/[-0-9]+\\s-*done\\]\\s-*$")
                      (let ((new-content (concat (match-string 1) counter-str)))
                        (delete-region (point) line-end)
                        (insert new-content))))))))))))

    ;; Run header number cleanup before save
    (defcustom my/markdown-cleanup-header-numbers-files nil
      "List of file name patterns for auto-cleanup of header numbers before save.
Each element is a regexp matched against `buffer-file-name'.
When a matching markdown buffer is saved,
`my/markdown-cleanup-heder-list-numbers-level' runs automatically.

Example:
  (setq my/markdown-cleanup-header-numbers-files
        \\='(\"TODO\\\\.md\" \"PLAN\\\\.md\" \"project-.*\\\\.md\"))"
      :type '(repeat regexp)
      :group 'markdown)

    (setq my/markdown-cleanup-header-numbers-files
          '("TODO\\.md"))

    (defun my/markdown-cleanup-header-numbers-before-save ()
      "Run header number cleanup before save if file matches configured patterns."
      (when (and (derived-mode-p 'markdown-mode)
                 buffer-file-name
                 my/markdown-cleanup-header-numbers-files
                 (cl-some (lambda (pattern)
                            (string-match-p pattern buffer-file-name))
                          my/markdown-cleanup-header-numbers-files))
        (my/markdown-cleanup-heder-list-numbers-level)))
    (add-hook 'before-save-hook #'my/markdown-cleanup-header-numbers-before-save)

;;;; markdown-mode-map
    (bind-keys
     :map markdown-mode-map
     ;; Mimicking the org-export style bindings
     ("C-c C-e o" . gk-markdown-preview-buffer)
     ("C-c C-e t". orgtbl-send-table)
     ("C-c C-s N" . my/markdown-cleanup-heder-list-numbers-level)
     ("C-c C-s n" . markdown-cleanup-list-numbers)
     ("M-p" . markdown-previous-visible-heading)
     ("M-n" . markdown-next-visible-heading))))


(provide 'setup-markdown)


;;; Notes
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
