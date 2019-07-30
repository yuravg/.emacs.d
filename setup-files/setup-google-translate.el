
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
          "Revert translate the word at point or the words in the active region") ;overwrite doc
        (defalias 'gtq 'google-translate-query-translate)
        (defalias 'gtqr 'google-translate-query-translate-reverse
          "Revert interactively translate text with Google Translate") ;overwrite doc
        ))

    ;; Fix error message: Failed to search TKK
    ;; https://github.com/atykhonov/google-translate/issues/52
    (when (and (string-match "0.11.14"
                             (google-translate-version))
               (>= (time-to-seconds)
                   (time-to-seconds
                    (encode-time 0 0 0 23 9 2018))))
      (defun google-translate--get-b-d1 ()
        ;; TKK='427110.1469889687'
        (list 427110 1469889687)))))


(provide 'setup-google-translate)

;; TIPS
;;
;; M-x google-translate-at-point
;; M-x google-translate-at-point-reverse
;; M-x google-translate-query-translate
;; M-x google-translate-query-translate-reverse
