;; Copyright © 2016, 2017, 2018, 2019 Emmanuel Roubion
;;
;; Author: Emmanuel Roubion
;; URL: https://github.com/maanuair/dotfiles

;; This file is part of Emmanuel's Roubion dot files, released under
;; the MIT License as published by the Massachusetts Institute of Technology
;;
;; These dotfiles are distributed in the hope they wil lbe useful, but
;; without any warranty. See the MIT License for more details
;;
;; You should have received a copy of the MIT License along with this file.
;; If not, see https://opensource.org/licenses/mit-license.php

;; This file is NOT part of GNU Emacs.

;; Hmmmmmm, use instead https://github.com/jwiegley/use-package ?

;; Initialize the env vars from shell, when started un GUI mode from macOS and GNU/Linux
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Automatic install of packages
(defvar
  my-packages
  '(
     adaptive-wrap
     all-the-icons
     all-the-icons-dired
     ;; bug-hunter
     counsel
     doom-modeline
     editorconfig
     emmet-mode
     exec-path-from-shell
     free-keys
     groovy-mode
     hackernews
     js2-mode
     json-mode
     lua-mode
     markdown-mode
     magit
     mocha
     org-bullets
     org-cliplink
     org-kanban
     ox-hugo
     pdf-tools
     restclient
     slime
     solarized-theme
     web-mode
     zenburn-theme
     )
  "The list of packages that should be installed, and verified as such at startup")

(defvar my-missing-packages '()
  "List that is populated at each startup, that will contain the list of packages that need to be installed.")

;; Populate the list of missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (add-to-list 'my-missing-packages p)))

;; Check internet connection
(setq my-onlinep nil)
(unless
  (condition-case nil
    (delete-process
      (make-network-process
        :name "my-check-internet"
        :host "elpa.gnu.org"
        :service 80))
    (error t))
  (setq my-onlinep t))

;; Automatically install the missing packages
(when (and my-missing-packages my-onlinep)
  (message "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (dolist (p my-missing-packages)
    (message "Installing `%s' .." p)
    (package-install p))
  (setq my-missing-packages '()))

(provide 'my-packages)
