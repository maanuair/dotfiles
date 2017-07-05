;; Behaviors
;;;;;;;;;;;;

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)

;; Use recent files
(require 'recentf)
(recentf-mode 1)

;; Use editor config
(require 'editorconfig)
(editorconfig-mode 1)

;; Use ido-mode
(require 'ido)
(ido-mode t)

;; Customize behavior
(load-library "paren")    ;; We use paren lib...
(show-paren-mode 1 )      ;; Visually match parentheses

;; Custom keyboard shortcuts
(global-set-key [(meta g)] 'goto-line)
(global-set-key (quote [f5]) 'reformat)
(global-set-key (quote [f12]) 'auto-revert-tail-mode)

;; Specific mac behaviors
(when (is-mac)
  (setq
   delete-by-moving-to-trash t      ;; Move to trash when deleting stuff
   trash-directory "~/.Trash/"      ;; Trash folder on OSX is ~/.Trash/)
   mac-right-option-modifier  'none ;; Make the right option (Alt) key native
   ))

;; Specific Win behaviors
(when (is-win)
  (setq
   delete-by-moving-to-trash t     ;; Move to trash when deleting stuff
   ))

;; Enable back the commands considered confusing to upper/lower case region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Appearance
;;;;;;;;;;;;;;

(setq
 apropos-do-all t               ;; More extensive search in apropos
 initial-scratch-message nil    ;; Remove scratch message
 inhibit-startup-screen t       ;; Remove start-up screen
 font-lock-maximum-decoration t ;; Use maximum decoration
 frame-title-format (list '(buffer-file-name "%f" (dired-directory dired-directory "%b")) " (" user-login-name "@" system-name ")") ;; Sets the frame title
 )
(column-number-mode 1)        ;; Display column number
(display-battery-mode 1)      ;; Show battery level
(global-hl-line-mode 1)       ;; Highlight current line
(global-font-lock-mode t)     ;; Toggle font Lock mode in all buffers
(size-indication-mode 1)      ;; Show size of buffer
(toggle-indicate-empty-lines) ;; Show empty lines
(tool-bar-mode -1)            ;; Remove toolbar
(transient-mark-mode t)       ;; Highlight active region

;; Use visible bell, customized
(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq
 ring-bell-function 'my-terminal-visible-bell
 visible-bell t
 )

;; Use solarized theme, but stick to my preferred fonts
(load-theme 'solarized-dark t)
(when (is-mac)
  ;;(add-to-list 'default-frame-alist '(font . "Monaco-11"))
  ;;(set-face-attribute 'default t :font "Monaco-11"))
  (add-to-list 'default-frame-alist '(font . "Cousine for Powerline-14"))
  (set-face-attribute 'default t :font "Cousine for Powerline-14"))
(when (is-win)
  (add-to-list 'default-frame-alist '(font . "Inconsolata-12"))
  (set-face-attribute 'default t :font "Inconsolata-12"))

;; Maggit shortcuts
(global-set-key (kbd "C-x g") 'magit-status)

;; Set up emmet-mode
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(provide 'my-ui)
				   
