
;; Google-translate

;; https://github.com/atykhonov/google-translate
(use-package google-translate
  :custom
  (google-translate-backend-method 'curl)
  :config
  (progn
    ;; To fix error: google-translate--search-tkk: Search failed: ",tkk:'"
    ;; https://github.com/atykhonov/google-translate/issues/52#issuecomment-727920888
    (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

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
          "Revert interactively translate text with Google Translate")

        ;; https://github.com/atykhonov/google-translate/issues/98#issuecomment-562870854
        ;; Debugger entered--Lisp error: (args-out-of-range [] 1)
        (defun google-translate-json-suggestion (json)
          "Retrieve from JSON (which returns by the
`google-translate-request' function) suggestion. This function
does matter when translating misspelled word. So instead of
translation it is possible to get suggestion."
          (let ((info (aref json 7)))
            (if (and info (> (length info) 0))
                (aref info 1)
              nil)))))))


(provide 'setup-google-translate)

;; TIPS
;;
;; M-x google-translate-at-point
;; M-x google-translate-at-point-reverse
;; M-x google-translate-query-translate
;; M-x google-translate-query-translate-reverse
