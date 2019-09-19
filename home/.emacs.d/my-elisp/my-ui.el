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

;; Appearance
;;;;;;;;;;;;;;

;; A customized visible bell
(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

;; My preferred set up
(setq
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
  mouse-buffer-menu-mode-mult 20               ;; Do not split the mouse buffer menu by major mode
  ring-bell-function 'my-terminal-visible-bell ;; Plug my customized visible bell
  sentence-end-double-space nil                ;; Sentences end with a single space
  show-paren-delay 0                           ;; Highlight parenthesis without delay
  show-paren-style 'expression                 ;; Highlight the matched expression
  visible-bell t                               ;; Use visible bell
  )
(column-number-mode 1)         ;; Display column number
(doom-modeline-mode 1)         ;; Use doom modeline
(global-hl-line-mode 1)        ;; Highlight current line
(global-font-lock-mode 1)      ;; Toggle font Lock mode in all buffers
(recentf-mode 1)               ;; Use recent files
(show-paren-mode 1)            ;; Visually match parentheses
(size-indication-mode 0)       ;; Do not show size of buffer
(toggle-indicate-empty-lines)  ;; Show empty lines
(tool-bar-mode 0)              ;; Remove toolbar
(toggle-scroll-bar 0)          ;; Remove scrollbar
(transient-mark-mode 1)        ;; Highlight active region

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


;;;;;;;;;;;;;;;;

;; Auto reformat on save
;; (require 'my-functions)
;; (add-hook 'before-save-hook 'reformat)
;; => Disabled, since cumbersome in some cases. Use [f5] shortcut instead :)

;; Automatically updates copyright updates on save
(add-hook 'before-save-hook 'copyright-update)

;; Make the default frame maximized at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq
  apropos-do-all t             ;; More extensive search in apropos
  delete-by-moving-to-trash t  ;; Move to OS' trash when deleting stuff
  vc-follow-symlinks t         ;; VC follows the link and visits the real file
  )
(delete-selection-mode 1)     ;; Delete text when typing over selection.
(editorconfig-mode 1)         ;; Use editor config
(ido-mode t)                  ;; Use ido
(save-place-mode 1)           ;; Save point position between sessions

;; Enable back the commands considered confusing to upper/lower case region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Global custom keyboard shortcuts
(global-set-key (kbd "M-g")    'goto-line)
(global-set-key (kbd "<f5>")   'reformat)
(global-set-key (kbd "<f6>")   'recompile)
(global-set-key (kbd "S-<f6>") 'next-error)
(global-set-key (kbd "<f12>")  'auto-revert-tail-mode)
(global-set-key (kbd "C-x g")  'magit-status)
(global-set-key (kbd "C-. i") (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))
(global-set-key (kbd "C-. d") (lambda () (interactive) (find-file (expand-file-name "my-dirs.el" my-elisp-dir))))
(global-set-key (kbd "C-. f") (lambda () (interactive) (find-file (expand-file-name "my-functions.el" my-elisp-dir))))
(global-set-key (kbd "C-. o") (lambda () (interactive) (find-file (expand-file-name "my-org.el" my-elisp-dir))))
(global-set-key (kbd "C-. p") (lambda () (interactive) (find-file (expand-file-name "my-packages.el" my-elisp-dir))))
(global-set-key (kbd "C-. u") (lambda () (interactive) (find-file (expand-file-name "my-ui.el" my-elisp-dir))))

;; Some modes settings
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start emmet on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; And on css modes as well

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
