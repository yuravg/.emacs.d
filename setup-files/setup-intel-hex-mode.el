
;; Intel-hex-mode

(use-package intel-hex-mode
  :load-path "elisp/manually-synced/intel-hex-mode"
  :config
  (progn
    (setq intel-hex-font-lock-keywords
          '(("^\\:" . font-lock-preprocessor-face) ;Start code
            ("^\\:\\([0-9A-Fa-f]\\{2\\}\\)" 1 font-lock-string-face) ;Byte count
            ("^\\:[0-9A-Fa-f]\\{2\\}\\([0-9A-Fa-f]\\{4\\}\\)" 1 font-lock-function-name-face) ;Address
            ("^\\:[0-9A-Fa-f]\\{6\\}\\([0-9A-Fa-f]\\{2\\}\\)" 1 font-lock-comment-delimiter-face) ;Record type
            ("[^0-9A-Fa-f]+" . font-lock-warning-face) ;Data
            ("\\([0-9A-Fa-f]\\{2\\}\\)$" 1 font-lock-keyword-face))))) ;Checksum


(provide 'setup-intel-hex-mode)

;; Notes
;; Example:
;; :0300300002337A1E
;; Start code | Byte count | Address | Record type | Data   | Checksum
;; :            03           0030      00            02337A   1E
