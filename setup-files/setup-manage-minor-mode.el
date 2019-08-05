;; Time-stamp: <2016-05-19 22:17:09 kmodi>

;; manage-minor-mode
;; https://github.com/ShingoFukuyama/manage-minor-mode

(use-package manage-minor-mode
  :defer t
  :bind (:map manage-minor-mode-map
         ("q" . modi/quit-and-kill-window)))


(provide 'setup-manage-minor-mode)
