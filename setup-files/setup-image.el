;; Time-stamp: <2019-07-30 14:45:04 kmodi>

(use-package image-mode
  :defer t
  :bind
  (:map image-mode-map
   ("C-c C-h" . my/plantuml-toggle-source-diagram)))

(provide 'setup-image)
