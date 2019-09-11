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

; Some global custom shortcuts
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;; Some shortcuts to load quiclky my personal PARA org files
(setq
  my-perso-org-dir (expand-file-name "~/Org/Perso")
  my-work-org-dir (expand-file-name "~/Org/Work")
  )
(global-set-key (kbd "C-c p p") (lambda () (interactive) (find-file (expand-file-name "projects.org"  my-perso-org-dir))))
(global-set-key (kbd "C-c p a") (lambda () (interactive) (find-file (expand-file-name "areas.org"     my-perso-org-dir))))
(global-set-key (kbd "C-c p r") (lambda () (interactive) (find-file (expand-file-name "resources.org" my-perso-org-dir))))
(global-set-key (kbd "C-c p z") (lambda () (interactive) (find-file (expand-file-name "archives.org"  my-perso-org-dir))))
(global-set-key (kbd "C-c p s") (lambda () (interactive) (find-file (expand-file-name "sample.org"    my-perso-org-dir))))
;; Same for work org files
(global-set-key (kbd "C-c w p") (lambda () (interactive) (find-file (expand-file-name "projects.org"  my-work-org-dir))))
(global-set-key (kbd "C-c w a") (lambda () (interactive) (find-file (expand-file-name "areas.org"     my-work-org-dir))))
(global-set-key (kbd "C-c w r") (lambda () (interactive) (find-file (expand-file-name "resources.org" my-work-org-dir))))
(global-set-key (kbd "C-c w z") (lambda () (interactive) (find-file (expand-file-name "archives.org"  my-work-org-dir(global-set-key (kbd "C-c w s") (lambda () (interactive) (find-file (expand-file-name "sample.org"    my-work-org-dir))))
))))

;; Some Org mode settings
(setq
  org-descriptive-links t                                 ;; Show hyperlinks decorated (not plain text)
  org-log-done t                                          ;; Insert the timestamp when a task is marked DONE
  org-startup-indented 'indet                             ;; Prefer the alternate stars and indent. scheme
  org-agenda-files '("~/Org/Perso/" "~/Org/Work/")        ;; Make available all .org files for C-c C-w (org-refile)
  org-refile-targets '(                                   ;; Specify targets for refile
                        (nil :maxlevel . 4)
                        (org-agenda-files :maxlevel . 4))
  org-refile-use-outline-path 'file                       ;; Include the file name (without directory) into the path
  )

;; We assume here the theme has already been loaded, and we specify org-mode level colors
(defun my-org-mode-hook ()
  ;; Don't override document title height
  (set-face-attribute 'org-document-title nil :height 1.0)

  ;; Use blueish links
  (set-face-attribute 'org-link nil :foreground "DodgerBlue2")

  ;; Use a more fainted keywords
  ;; set-face-attribute 'org-document-info-keyword nil :foreground "grey5")

  ;; Level colors
  (set-face-attribute 'org-level-1 nil :foreground "RoyalBlue1")
  (set-face-attribute 'org-level-2 nil :foreground "orange3")
  (set-face-attribute 'org-level-3 nil :foreground "SpringGreen4")
  (set-face-attribute 'org-level-4 nil :foreground "firebrick3")
  (set-face-attribute 'org-level-5 nil :foreground "cyan3")
  (set-face-attribute 'org-level-6 nil :foreground "dark sea green")
  (set-face-attribute 'org-level-7 nil :foreground "turquoise4")
  (set-face-attribute 'org-level-8 nil :foreground "DodgerBlue4")
  )

;; We use org-bullets-mode, and we choose these bullets


;; Automagically loads our custom settings, and org-bullets
(add-hook 'org-load-hook #'my-org-mode-hook)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))

(provide 'my-org)
