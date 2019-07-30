
;; Highlight-indent-guides

;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :config
  (progn
    (setq highlight-indent-guides-method 'column)

    (set-face-background 'highlight-indent-guides-odd-face "gray92")
    (set-face-background 'highlight-indent-guides-even-face "gray90")

    (defalias 'ig 'highlight-indent-guides-mode)))


(provide 'setup-highlight-indent-guides)
