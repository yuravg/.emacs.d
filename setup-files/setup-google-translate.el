
;; Google-translate

;; https://github.com/atykhonov/google-translate
(use-package google-translate
  :config
  (progn
    (use-package google-translate-default-ui
      :config
      (progn
        (setq google-translate-default-source-language "en")
        (setq google-translate-default-target-language "ru")
        (defalias 'gt 'google-translate-at-point)
        (defalias 'gtr 'google-translate-at-point-reverse
          ;; replacement of doc-string
          "Revert translate the word at point or the words in the active region")
        (defalias 'gtq 'google-translate-query-translate)
        (defalias 'gtqr 'google-translate-query-translate-reverse
          ;; replacement of doc-string
          "Revert interactively translate text with Google Translate")))))


(provide 'setup-google-translate)

;; TIPS
;;
;; M-x google-translate-at-point
;; M-x google-translate-at-point-reverse
;; M-x google-translate-query-translate
;; M-x google-translate-query-translate-reverse
