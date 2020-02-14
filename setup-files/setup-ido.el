;; Time-stamp: <2016-05-19 22:02:53 kmodi>

;; Interactively Do Things
;; http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; https://github.com/magnars/.emacs.d/blob/master/setup-ido.el

(use-package ido
  :preface
  (progn
    ;; `defvar's to prevent compile warnings
    (defvar ido-cur-item               nil)
    (defvar ido-default-item           nil)
    (defvar predicate                  nil)
    (defvar inherit-input-method       nil)
    (defvar ido-cur-list               nil)
    (defvar ido-context-switch-command nil))
  :defer t
  ;; :init
  ;; (progn
  ;;   (setq ido-enable-flex-matching  t) ; enable fuzzy search
  ;;   (setq ido-everywhere            t)
  ;;   (setq ido-create-new-buffer     'always) ; create a new buffer if no buffer matches substring
  ;;   (setq ido-file-extensions-order '(".sv" ".v" ".svh" ".tv" ".m" ".c" ".cpp" ".el"))
  ;;   ;;  customize the order in which files are sorted when Ido displays them in
  ;;   ;; the minibuffer. There are certain file extensions I use more than others,
  ;;   ;; so I tell Ido to emphasize those
  ;;   (setq ido-use-filename-at-point 'guess) ; find file at point using ido
  ;;   ;; look into other directories if the entered filename doesn't exist
  ;;   ;; in current directory ido-auto-merge-work-directories-length -1
  ;;   ;; do NOT look into other directories if the entered filename doesn't
  ;;   ;; exist in current directory
  ;;   (setq ido-auto-merge-work-directories-length 0))
  :config
  (progn
    (ido-mode 1)

    ;; The following command disables the `ivy-mode' after update this package:
    ;; Re-enable `ido-mode' again after ensuring that `ivy-mode' is off
    ;; (with-eval-after-load 'ivy
    ;;   (ivy-mode -1)
    ;;   ;; Enable ido
    ;;   (ido-mode 1))

    ;; Use flx-ido for better flex matching between words
    (use-package flx-ido
      :ensure t
      :config
      (progn
        (setq ido-use-faces nil) ; disable ido faces to see flx highlights
        (flx-ido-mode 1)))

    (use-package ido-vertical-mode
      :ensure t
      :config
      (progn
        (ido-vertical-mode 1))) ; flx-ido looks better with ido-vertical-mode

    (defun endless/ido-bury-buffer-at-head ()
      "Bury the buffer at the head of `ido-matches'.
http://endlessparentheses.com/Ido-Bury-Buffer.html
This is merged into emacs 25.0."
      (interactive)
      (let ((enable-recursive-minibuffers t)
            (buf (ido-name (car ido-matches)))
            (nextbuf (cadr ido-matches)))
        (when (get-buffer buf)
          ;; If next match names a buffer use the buffer object;
          ;; buffer name may be changed by packages such as
          ;; uniquify.
          (when (and nextbuf (get-buffer nextbuf))
            (setq nextbuf (get-buffer nextbuf)))
          (bury-buffer buf)
          (if (bufferp nextbuf)
              (setq nextbuf (buffer-name nextbuf)))
          (setq ido-default-item nextbuf
                ido-text-init ido-text
                ido-exit 'refresh)
          (exit-minibuffer))))

    (defun ido-define-keys ()
      (unbind-key "C-a" ido-completion-map) ; default binding: `ido-toggle-ignore'
      (bind-keys
       :map ido-completion-map
       ;; C-n/p  and up/down keys are more intuitive in vertical layout
       ("C-n"    . ido-next-match)
       ("<down>" . ido-next-match)
       ("C-p"    . ido-prev-match)
       ("<up>"   . ido-prev-match)
       ("C-f"    . ido-magic-forward-char)
       ("C-b"    . ido-magic-backward-char)
       ("C-i"    . ido-toggle-ignore)
       ("C-j"    . ido-exit-minibuffer)))
    (add-hook 'ido-setup-hook #'ido-define-keys)))


(provide 'setup-ido)

;; Default Ido Key Map
;;
;; Basic map
;; | C-a     | 'ido-toggle-ignore              |
;; | C-c     | 'ido-toggle-case                |
;; | C-e     | 'ido-edit-input                 |
;; | Tab     | 'ido-complete                   |
;; | Space   | 'ido-complete-space             |
;; | C-j     | 'ido-select-text                |
;; | C-m     | 'ido-exit-minibuffer            |
;; | C-p     | 'ido-toggle-prefix (OVERRIDDEN) |
;; | C-r     | 'ido-prev-match                 |
;; | C-s     | 'ido-next-match                 |
;; | C-t     | 'ido-toggle-regexp              |
;; | C-z     | 'ido-undo-merge-work-directory  |
;; | C-Space | 'ido-restrict-to-matches        |
;; | M-Space | 'ido-take-first-match           |
;; | C-@     | 'ido-restrict-to-matches        |
;; | Right   | 'ido-next-match                 |
;; | Left    | 'ido-prev-match                 |
;; | ?       | 'ido-completion-help            |
;;
;; Magic commands.
;; | C-b | 'ido-magic-backward-char |
;; | C-f | 'ido-magic-forward-char  |
;; | C-d | 'ido-magic-delete-char   |
;;
;; File and directory map
;; | C-x C-b                      | 'ido-enter-switch-buffer                 |
;; | C-x C-f                      | 'ido-fallback-command                    |
;; | C-x C-d                      | 'ido-enter-dired                         |
;; | Down                         | 'ido-next-match-dir                      |
;; | Up                           | 'ido-prev-match-dir                      |
;; | M-Up                         | 'ido-prev-work-directory                 |
;; | M-Down                       | 'ido-next-work-directory                 |
;; | Backspace                    | 'ido-delete-backward-updir               |
;; | Delete                       | 'ido-delete-backward-updir               |
;; | [remap delete-backward-char] | 'ido-delete-backward-updir) ; B          |
;; | [remap backward-kill-word]   | 'ido-delete-backward-word-updir)  ; M-DE |
;; | C-Backspace                  | 'ido-up-directory                        |
;; | C-l                          | 'ido-reread-directory                    |
;; | M-d                          | 'ido-wide-find-dir-or-delete-dir         |
;; | M-b                          | 'ido-push-dir                            |
;; | M-v                          | 'ido-push-dir-first                      |
;; | M-f                          | 'ido-wide-find-file-or-pop-dir           |
;; | M-k                          | 'ido-forget-work-directory               |
;; | M-m                          | 'ido-make-directory                      |
;; | M-n                          | 'ido-next-work-directory                 |
;; | M-o                          | 'ido-prev-work-file                      |
;; | M-C-o                        | 'ido-next-work-file                      |
;; | M-p                          | 'ido-prev-work-directory                 |
;; | M-s                          | 'ido-merge-work-directories              |
;;
;; File only map
;; | C-k | 'ido-delete-file-at-head                                         |
;; | C-o | 'ido-copy-current-word                                           |
;; | C-w | 'ido-copy-current-file-name (Insert file name of current buffer) |
;; | M-l | 'ido-toggle-literal                                              |
;;
;; Buffer map
;; | C-x C-f | 'ido-enter-find-file        |
;; | C-x C-b | 'ido-fallback-command       |
;; | C-k     | 'ido-kill-buffer-at-head    |
;; | C-o     | 'ido-toggle-virtual-buffers |
