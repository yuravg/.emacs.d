;; Time-stamp: <2016-11-20 22:24:32 kmodi>

;; server/daemon setup

(use-package server
  :init
  (progn
    (setq server-use-tcp t)
    ;; `server-auth-dir' is used for server authentication files only if
    ;; `server-use-tcp' is non-nil.
    ;; On Windows, also set the EMACS_SERVER_FILE environment variable to
    ;; point to the `server' file. For example, for emacs 25.1, that location
    ;; would be "PATH\TO\.emacs.d\server_25_1_HOSTNAME\server".
    (setq server-auth-dir
          (let ((dir (concat user-emacs-directory
                             "server"
                             "/"))) ; must end with /
            (make-directory dir :parents)
            dir)))
  :config
  (progn
    ;; http://stackoverflow.com/a/1566618/1219634
    ;; Suppress error "directory  ~/.emacs.d/server is unsafe". It is needed
    ;; for the server to start on Windows.
    ;; Below hack should not be needed if the User is the Owner of the
    ;; `server-auth-dir'. To check that, Right-click on that server directory
    ;; > Properties > Advanced > Owner, and check that the owner is the User
    ;; and not the Administrator. If that's not the case and you do not have
    ;; admin rights to fix that, below hack just works.
    (defun server-ensure-safe-dir (dir) "Noop" t)

    ;; Start a server if (server-running-p) does not return t (e.g. if it
    ;; returns nil or :other)
    (or (eq (server-running-p) t)
        (server-start))))


(provide 'setup-server)
