
;; Docker

;; https://github.com/Silex/docker.el
(use-package docker
  :ensure t
  :bind (("C-c M-d" . docker)))

;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode)

(provide 'setup-docker)

;; TIPS
;; |---------+----------------------|
;; | Binding | Description          |
;; |---------+----------------------|
;; | ?       | List actions         |
;; | l       | Configure listing    |
;; | m       | Mark item            |
;; | u       | Unmark item          |
;; | t       | Toggle marks         |
;; | U       | Unmark all           |
;; | s       | Sort                 |
;; | * r     | Mark items by regexp |
;; | <       | Shrink column        |
;; | >       | Enlarge column       |
;; | C-c C-e | Export to csv        |
;; |---------+----------------------|
