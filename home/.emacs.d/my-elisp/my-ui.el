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

;; Appearance
;;;;;;;;;;;;;;

;; A customized visible bell
(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

;; My preferred set up
(setq
  apropos-do-all t                      ;; More extensive search in apropos
  delete-by-moving-to-trash t           ;; Move to OS' trash when deleting stuff
  display-time-24hr-format t            ;; Time in 24h format
  doom-modeline-enable-word-count t     ;; Add a word count selection-info modeline segment.
  doom-modeline-major-mode-color-icon t ;; Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.
  doom-modeline-minor-modes t           ;; Whether display minor modes or not. Non-nil to display in mode-line.
  echo-keystrokes 0.5                   ;; Show keystrokes right away
  font-lock-maximum-decoration t        ;; Use maximum decoration
  frame-title-format (                  ;; Sets the frame title
                       list
                       '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
                       " (" user-login-name "@" system-name  ")"
                       )
  inhibit-startup-screen t                     ;; Remove start-up screen
  initial-scratch-message nil                  ;; Remove scratch message
  ivy-use-virtual-buffers t                    ;; Add recent files and bookmarks to ‘ivy-switch-buffer
  ivy-count-format "(%d/%d) "                  ;; Display the current candidate count for `ivy-read' to display both the index and the count.
  mouse-buffer-menu-mode-mult 20               ;; Do not split the mouse buffer menu by major mode
  ring-bell-function 'my-terminal-visible-bell ;; Plug my customized visible bell
  sentence-end-double-space nil                ;; Sentences end with a single space
  sgml-quick-keys 'close                       ;; SGML mode will auto close element when typin '</'
  show-paren-delay 0                           ;; Highlight parenthesis without delay
  show-paren-style 'expression                 ;; Highlight the matched expression
  vc-follow-symlinks t                         ;; VC follows the link and visits the real file
  visible-bell t                               ;; Use visible bell
  )
(column-number-mode 1)         ;; Display column number
(delete-selection-mode 1)      ;; Delete text when typing over selection.
(doom-modeline-mode 1)         ;; Use doom modeline
(editorconfig-mode 1)          ;; Use editor config
(global-hl-line-mode 1)        ;; Highlight current line
(global-font-lock-mode 1)      ;; Toggle font Lock mode in all buffers
(ivy-mode t)                   ;; Giving Ivy a try :)
(recentf-mode 1)               ;; Use recent files
(save-place-mode 1)            ;; Save point position between sessions
(show-paren-mode 1)            ;; Visually match parentheses
(size-indication-mode 0)       ;; Do not show size of buffer
(toggle-indicate-empty-lines)  ;; Show empty lines
(tool-bar-mode 0)              ;; Remove toolbar
(toggle-scroll-bar 1)          ;; Use vertical scrollbar
(transient-mark-mode 1)        ;; Highlight active region

;; Initialize the dashboard :)
;; (require 'dashboard)
;; (setq
;;   dashboard-banner-logo-title "Welcome to the thermonuclear word processor."
;;   dashboard-items '(
;;                      (recents  . 10)
;;                      (bookmarks . 5)
;;                      (agenda . 20)
;;                      (registers . 5))
;;   dashboard-set-heading-icons t
;;   dashboard-set-file-icons    t
;;   dashboard-set-navigator     t
;;   dashboard-startup-banner    'logo
;;   )
;; (dashboard-setup-startup-hook)

;; My preferred theme and its tweaks: Solarized
(setq
  solarized-use-variable-pitch nil   ;; No proportional font for org-mode headings
  solarized-scale-org-headlines nil  ;; No need to change their heights too
  )
(load-theme 'solarized-dark t)
(set-face-attribute
  'show-paren-match-expression nil
  :inherit nil
  :background (face-background 'highlight))

;; Some modes settings
(add-hook 'before-save-hook 'copyright-update)         ;; Automatically updates copyright updates on save
(add-hook 'css-mode-hook    'emmet-mode)               ;; And on css modes as well
(add-hook 'dired-mode-hook  'all-the-icons-dired-mode) ;; Dired support for all-the-icons
(add-hook 'sgml-mode-hook   'emmet-mode)               ;; Auto-start emmet on any markup modes

;; Make the default frame maximized at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable js2-mode for editing JS files
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  )

;; Enable web-mode for editing HTML files
(use-package web-mode
  :mode
  ("\\.html\\'" . web-mode)
  ("\\.ejs\\'" . web-mode)
  )

;; A few UI tweaks for treemacs
(use-package treemacs
  :custom-face
  (treemacs-root-face ((t (:height 1.0))))
  (treemacs-directory-face ((t (:height 1.0))))
  (treemacs-file-face ((t (:height 1.0))))
  )

;; Make sure PDF will be opened with pdf-tools
(pdf-loader-install)

;;;;;;;;;;;;;;;;

;; Global custom keyboard shortcuts
(global-set-key (kbd "M-g")    'goto-line)
(global-set-key (kbd "<f5>")   'reformat)
(global-set-key (kbd "<f6>")   'recompile)
(global-set-key (kbd "S-<f6>") 'next-error)
(global-set-key (kbd "<f12>")  'auto-revert-tail-mode)
(global-set-key (kbd "C-x g")  'magit-status)
(global-set-key (kbd "C-x t")  'treemacs)
(global-set-key (kbd "C-x c")  'indium-connect)
(global-set-key (kbd "C-. i") (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))
(global-set-key (kbd "C-. d") (lambda () (interactive) (find-file (expand-file-name "my-dirs.el" my-elisp-dir))))
(global-set-key (kbd "C-. f") (lambda () (interactive) (find-file (expand-file-name "my-functions.el" my-elisp-dir))))
(global-set-key (kbd "C-. o") (lambda () (interactive) (find-file (expand-file-name "my-org.el" my-elisp-dir))))
(global-set-key (kbd "C-. p") (lambda () (interactive) (find-file (expand-file-name "my-packages.el" my-elisp-dir))))
(global-set-key (kbd "C-. u") (lambda () (interactive) (find-file (expand-file-name "my-ui.el" my-elisp-dir))))

;; Enable back the commands considered confusing to upper/lower case region
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)

;; UI tweaks on macOS
;;;;;;;;;;;;;;;;;;;;;
(when (is-mac)
  (setq
    trash-directory "~/.Trash/"      ;; Trash folder on OSX is ~/.Trash/)
    mac-right-option-modifier  'none ;; Make the right option (Alt) key native
    )
  ;; Despite the theme, stick to my preferred fonts
  (add-to-list 'default-frame-alist '(font . "Cousine for Powerline-14"))
  (set-face-attribute 'default t :font "Cousine for Powerline-14")

  )

;; UI tweak on Windows
;;;;;;;;;;;;;;;;;;;;;;
(when (is-win)
  ;; Despite the theme, stick to my preferred fonts
  (add-to-list 'default-frame-alist '(font . "Inconsolata-12"))
  (set-face-attribute 'default t :font "Inconsolata-12")
  )

(provide 'my-ui)
