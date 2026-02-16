;; Time-stamp: <2026-02-16 16:35:45 gritsenko>

(use-package image-mode
  :defer t
  :bind
  (:map image-mode-map
   ("C-c C-h" . my/plantuml-toggle-source-diagram)
   ("C-+"     . my/image-zoom-in)
   ("C-="     . my/image-zoom-in)   ; alternative, easier on some keyboards
   ("C--"     . my/image-zoom-out)
   ("C-_"     . my/image-zoom-out)) ; fallback for C--
  :config
  (progn
    (defun my/image-zoom-in ()
      "Zoom in on image at point. Works for both file-backed and non-file images."
      (interactive)
      (if (image-at-point-p)
          (image-increase-size 0.2)
        (image-transform-set-scale 1.2)))

    (defun my/image-zoom-out ()
      "Zoom out on image at point. Works for both file-backed and non-file images."
      (interactive)
      (if (image-at-point-p)
          (image-decrease-size 0.2)
        (image-transform-set-scale (/ 1.0 1.2))))))

(provide 'setup-image)
