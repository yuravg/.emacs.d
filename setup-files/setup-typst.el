
;; Typst
;; https://github.com/typst/typst

;; https://github.com/kaction-emacs/typst-ts-mode
(use-package typst-ts-mode
  :ensure (:type git :host sourcehut :repo "meow_king/typst-ts-mode" :files (:defaults "*.el"))
  :custom
  ;; don't add "--open" if you'd like `watch` to be an error detector
  (typst-ts-mode-watch-options "--open")

  ;; experimental settings (I'm the main dev, so I enable these)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))


(provide 'setup-typst)
