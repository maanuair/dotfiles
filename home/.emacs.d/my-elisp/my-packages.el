;; Copyright © 2016, 2017, 2018, 2019, 2020 Emmanuel Roubion
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

;; We use the non stable MELPA source, since some packages are missing from MELPA Stable
;; (I'm looking at you, restclient, org-cliplink, allthe-icons-dired...)
(setq my-preferred-package-host "melpa.org")
(setq my-preferred-package-entry (cons "melpa" (concat "https://" my-preferred-package-host "/packages/")))
(add-to-list 'package-archives my-preferred-package-entry t)

;; We automatically install the missing packages  listed below.
;; This is expensive at first run.
(defvar
  my-packages
  '(
     adaptive-wrap
     all-the-icons
     all-the-icons-dired
     counsel
     dashboard
     doom-modeline
     editorconfig
     emmet-mode
     esup
     exec-path-from-shell
     free-keys
     groovy-mode
     hackernews
     indium		;; Reminder: npm install -g indium
     js2-mode
     json-mode
     logview
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
     treemacs
     use-package
     web-mode
     which-key
     zenburn-theme
     )
  "The list of packages that should be installed, and verified as such at startup")

(defvar my-missing-packages '()
  "List that is populated at each startup, that will contain the list of packages that need to be installed.")

;; Populate the list of missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (add-to-list 'my-missing-packages p)))

;; When there is missing package(s), check internet connection in order to see whether we update
(setq my-onlinep nil)
(when my-missing-packages
  (unless
    (condition-case nil
      (delete-process
        (make-network-process
          :name "my-check-internet"
          :host my-preferred-package-host
          :service 80))
      (error t))
    (setq my-onlinep t))
  )

;; Automatically install the missing packages
(when (and my-missing-packages my-onlinep)
  (message "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (dolist (p my-missing-packages)
    (message "Installing `%s' .." p)
    (package-install p))
  (setq my-missing-packages '()))

;; Now, we can initialize the env vars from shell, when emacs started in GUI mode from macOS or GNU/Linux
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'my-packages)
