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

;; Everything related to my Org mode usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A function to open my default org file
(defun load-my-org ()
  "Loads my org file, stored by default in \"~/org/index.org.\""
  (interactive)
  (find-file "~/org/index.org" nil)
  )

;; Some global custom shortcuts
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c o") 'load-my-org)

;; Some Org mode settings
(setq
  org-descriptive-links nil    ;; Show hyperlinks in plain text
  org-log-done t               ;; Insert the timestamp when a task is marked DONE
  org-startup-indented 'indet  ;; Prefer the alternate stars and indent. scheme
  )

(provide 'my-org)
