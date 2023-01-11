#!/usr/bin/env bash

# This is a script to update configuration files
# in 'manually-synced' directory

curl https://www.emacswiki.org/emacs/download/dired%2b.el > \
     dired-plus/dired+.el

curl https://raw.githubusercontent.com/flycheck/flycheck-google-cpplint/master/flycheck-google-cpplint.el > \
     flycheck-google-cpplint/flycheck-google-cpplint.el

curl https://www.emacswiki.org/emacs/download/header2.el > \
     header2/header2.el

curl https://www.emacswiki.org/emacs/download/help-fns%2b.el > \
     help-fns-plus/help-fns+.el

curl https://www.emacswiki.org/emacs/download/highlight.el > \
     highlight/highlight.el

curl https://www.emacswiki.org/emacs/download/hl-line%2b.el > \
     hl-line-plus/hl-line+.el

curl https://www.emacswiki.org/emacs/download/info%2b.el > \
     info-plus/info+.el
