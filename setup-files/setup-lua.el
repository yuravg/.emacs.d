;; LUA

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :config
  (progn
    (defun yura/lua-set-indentation ()
      "Customize the indentation for `lua-mode'."
      (setq tab-width 4
            lua-indent-level 4
            indent-tabs-mode nil))
    (add-hook 'lua-mode-hook #'yura/lua-set-indentation)))


(provide 'setup-lua)
