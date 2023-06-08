;; Time-stamp: <2015-09-15 11:03:30 kmodi>

;; Undo Tree
;; http://www.dr-qubit.org/emacs.php

(use-package undo-tree
  :config
  (progn
    (defun modi/undo-tree-enable-save-history ()
      "Enable auto saving of the undo history."
      (interactive)

      (setq undo-tree-auto-save-history t)

      ;; Compress the history files as .gz files
      ;; (advice-add 'undo-tree-make-history-save-file-name :filter-return
      ;;             (lambda (return-val) (concat return-val ".gz")))

      ;; Persistent undo-tree history across emacs sessions
      (setq modi/undo-tree-history-dir (let ((dir (concat user-emacs-directory
                                                          "undo-tree-history/")))
                                         (make-directory dir :parents)
                                         dir))
      (setq undo-tree-history-directory-alist `(("." . ,modi/undo-tree-history-dir)))

      (add-hook 'write-file-functions #'undo-tree-save-history-hook)
      (add-hook 'find-file-hook #'undo-tree-load-history-hook))

    (defun modi/undo-tree-disable-save-history ()
      "Disable auto saving of the undo history."
      (interactive)

      (setq undo-tree-auto-save-history nil)

      (remove-hook 'write-file-functions #'undo-tree-save-history-hook)
      (remove-hook 'find-file-hook #'undo-tree-load-history-hook))

    (modi/undo-tree-disable-save-history)

    (global-undo-tree-mode 1)))


(provide 'setup-undo-tree)

;; `undo-tree' package also remaps `undo' and `undo-only' to `undo-tree-undo'
;;
;; * `undo-tree-visualize' bindings
;;
;; C-x u - `undo-tree-visualize'
;;
;; | [remap previous-line] | undo-tree-visualize-undo |
;; | [remap next-line]     | undo-tree-visualize-redo |
;; | C-p                   | undo-tree-visualize-undo |
;; | C-n                   | undo-tree-visualize-redo |
;;
;; Horizontal motion keys switch branch
;; | [remap forward-char]  | undo-tree-visualize-switch-branch-right |
;; | [remap backward-char] | undo-tree-visualize-switch-branch-left  |
;; | C-f                   | undo-tree-visualize-switch-branch-right |
;; | C-b                   | undo-tree-visualize-switch-branch-left  |
;;
;; Paragraph motion keys undo/redo to significant points in tree
;; | [remap backward-paragraph] | undo-tree-visualize-undo-to-x |
;; | [remap forward-paragraph]  | undo-tree-visualize-redo-to-x |
;; | M-{                        | undo-tree-visualize-undo-to-x |
;; | M-}                        | undo-tree-visualize-redo-to-x |
;; | C-up                       | undo-tree-visualize-undo-to-x |
;; | C-down                     | undo-tree-visualize-redo-to-x |
;;
;; Mouse sets buffer state to node at click
;; | [mouse-1] | undo-tree-visualizer-mouse-set |
;;
;; Toggle timestamps
;; | t | undo-tree-visualizer-toggle-timestamps |
;;
;; Toggle diff
;; | d | undo-tree-visualizer-toggle-diff |
;;
;; Toggle selection mode
;; | s | undo-tree-visualizer-selection-mode |
;;
;; Horizontal scrolling may be needed if the tree is very wide
;; | , / < | undo-tree-visualizer-scroll-left  |
;; | . / > | undo-tree-visualizer-scroll-right |
;;
;; Vertical scrolling may be needed if the tree is very tall
;; | Page Down | undo-tree-visualizer-scroll-up   |
;; | Page Up   | undo-tree-visualizer-scroll-down |
;;
;; Quit/abort visualizer
;; | q   | undo-tree-visualizer-quit  |
;; | C-q | undo-tree-visualizer-abort |
