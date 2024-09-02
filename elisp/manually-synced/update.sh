#!/usr/bin/env bash

# This is a script to update configuration files
# in 'manually-synced' directory

wget -O - https://www.emacswiki.org/emacs/download/dired%2b.el > \
     dired-plus/dired+.el

wget -O - https://raw.githubusercontent.com/flycheck/flycheck-google-cpplint/master/flycheck-google-cpplint.el > \
     flycheck-google-cpplint/flycheck-google-cpplint.el

wget -O - https://www.emacswiki.org/emacs/download/header2.el > \
     header2/header2.el

wget -O - https://www.emacswiki.org/emacs/download/help-fns%2b.el > \
     help-fns-plus/help-fns+.el

wget -O - https://www.emacswiki.org/emacs/download/highlight.el > \
     highlight/highlight.el

wget -O - https://www.emacswiki.org/emacs/download/hl-line%2b.el > \
     hl-line-plus/hl-line+.el

wget -O - https://www.emacswiki.org/emacs/download/info%2b.el > \
     info-plus/info+.el
