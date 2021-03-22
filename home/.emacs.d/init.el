;;; init.el --- Emmanuel Roubion's personal init file -*- lexical-binding: t; -*-

;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Emmanuel Roubion

;; Author: Emmanuel Roubion
;; URL: https://github.com/maanuair/dotfiles

;; This file is part of Emmanuel's Roubion dot files, released under
;; the MIT License as published by the Massachusetts Institute of Technology

;;; Commentary:

;; These dotfiles are distributed in the hope they will be useful, but
;; without any warranty.  See the MIT License for more details.
;;
;; You should have received a copy of the MIT License along with this file.
;; If not, see https://opensource.org/licenses/mit-license.php

;; This file is NOT part of GNU Emacs.

;;; Code:

;; ======================================================================
;; Prologue
;; Startup time performance
;; ======================================================================


;; I followed some advices on https://blog.d46.us/advanced-emacs-startup/

;; My default, naked, Emacs startup time is ~ 0.4 seconds in GUI mode.
;; On macOS, run the following to discover it:
;;   open -n /Applications/Emacs.app --args -q --eval='(message "%s" (emacs-init-time))'
;; On other OSes:
;;   emacs -q --eval='(message "%s" (emacs-init-time))'

;; Currently, startup time is:
;;   <even before>: 1.86 seconds, with 5 Cs
;;   2020-09:       2.49 seconds, with 11 GCs
;;   2021-01:       5.26 seconds, with 11 GCs
;;   2021-03:       1.15 seconds, with 14 GCs (on Apple Silicon M1)

;; Make startup faster by reducing the frequency of GC.
(setq
  ;; The default is 800 kilobytes. Measured in bytes.
  gc-cons-threshold (* 50 1000 1024)

  ;; Portion of heap used for allocation. Defaults to 0.1.
  gc-cons-percentage 0.4)

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
	(lambda ()
	  (message "Emacs ready in %s with %d garbage collections."
		  (format "%.2f seconds"
			  (float-time
			    (time-subtract after-init-time before-init-time)))
		  gcs-done)))

;; Use a hook so we restore settings after init
(add-hook 'after-init-hook
  `(lambda ()
     (setq
       gc-cons-threshold  (* 800 1024)
       gc-cons-percentage 0.1)
     (garbage-collect)) t)

;; ======================================================================
;; Act I
;; Set up the load path
;; ======================================================================

(defconst my/elisp-dir
  (setq my/elisp-dir (expand-file-name "my-elisp/" user-emacs-directory))
  "Where lies my local elisp dir.")
(add-to-list 'load-path my/elisp-dir)

;; Add our git sub modules in load-path
(add-to-list 'load-path (expand-file-name "no-littering" my/elisp-dir))
(add-to-list 'load-path (expand-file-name "oblique-strategies" my/elisp-dir))

;; Reminder about Git submodules option, when cloning this project
;;   git clone --recurse-submodules git@github.com:maanuair/dotfiles.git
;;
;; When --recurse-submodules is forgotten, use:
;;   git submodule init && git submodule update
;;
;; Or simply:
;;   git submodule update --init

;; Bootstrap no-littering.el
(defconst my/no-littering-dir        (expand-file-name "../.emacs.runtime-dirs/" user-emacs-directory) "Location of the no-littering-dir package.")
(defconst no-littering-etc-directory (expand-file-name "etc/" my/no-littering-dir) "Where no-literring stores the configuration files.")
(defconst no-littering-var-directory (expand-file-name "var/" my/no-littering-dir) "Where no-literring stores the variable data.")
(require 'no-littering)

;; ======================================================================
;; Act II
;; Bootstrap package management
;; ======================================================================

(require 'package)

;; Add the stable & non-stable MELPA packages source
(add-to-list 'package-archives (cons "melpa-stable"   "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives (cons "melpa-unstable" "https://melpa.org/packages/") t)

;; A few package.el settings
(if init-file-debug
  (setq
    use-package-verbose            t
    use-package-expand-minimally   nil
    use-package-compute-statistics t
    debug-on-error                 t)
  (setq
    use-package-verbose            nil
    use-package-expand-minimally   t))

;; Initialize package.el
(unless package--initialized (package-initialize nil))

;; Install use-package if needed (e.g.: on first startup...)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Now we can manage packages with use-package.el
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(require 'use-package-ensure)
(setq
  use-package-always-ensure t               ; Always install packages automatically
  use-package-always-pin    "melpa-stable") ; Prefer stable packages

;; ======================================================================
;; Act III
;; Declaring my functions here
;; (flycheck will warn when defined in use-package clauses)
;; ======================================================================

;; Adapted from source: https://stackoverflow.com/a/11916238/3899249
(defvar my/buffer-kill-all-exceptions
  '( "\\`\\*scratch\\*.*\\'"
     "\\`\\*Messages\\*\\'"
     "\\` \\*Minibuf-[[:digit:]]+\\*\\'"
     "\\` \\*Echo Area [[:digit:]]+\\*\\'")
  "Exception list for `my/buffer-kill-all-but-exceptions'.")

(defun my/buffer-kill-all-but-exceptions ()
  "Kill all buffers except those in `my/buffer-kill-all-exceptions'."
  (interactive)
  (mapc (lambda (buf)
          (let
            ((buf-name (buffer-name buf)))
            (when
              (andT
                ;; if a buffer's name is enclosed by * with optional leading space characters
                ;; (string-match-p "\\` *\\*.*\\*\\'" buf-name)
                ;; The buffer must not be associated with a process
                (null
                  (get-buffer-process buf))
                ;; The buffer's name must not be in `my/buffer-kill-all-exceptions' var
                (null
                  (lambda
                    (exception)
                    (string-match-p exception buf-name)
                    (my/buffer-kill-all-exceptions)))
                (kill-buffer buf)))))
    (buffer-list)))

(defun my/buffer-new-scratch ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "*scratch*")))
    (set-buffer-major-mode buffer)
    (switch-to-buffer buffer)))

(defun my/find-bash-profile-file () "Visit my Bash profile."
  (interactive) (find-file "~/.profile"))

(defun my/find-emacs-init-file () "Visit my Emacs initialization file."
  (interactive) (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun my/find-hammerspoon-init-file () "Visit my Hammerspoon init file."
  (interactive) (find-file "~/.hammerspoon/init.lua"))

(defun my/find-shell-aliases-file () "Visit my shell aliases file."
  (interactive) (find-file "~/.shell_aliases"))

(defun my/find-shell-functions-file () "Visit my shell functions file."
  (interactive) (find-file "~/.shell_functions"))

(defun my/find-shell-local-script-file () "Visit my local shell file."
  (interactive) (find-file "~/.shell_local_script"))

(defun my/find-zshrc-file () "Visit my Zshrc."
  (interactive)
  (find-file "~/.zshrc"))

(defun my/is-macos ()
  "Return t when the system is a macOS."
	(interactive)
	(equal system-type 'darwin))

(defun my/is-win32 ()
  "Return t when the system is a Windows."
	(interactive)
	(equal system-type 'windows-nt))

(defun my/oblique-strategy ()  "Draw and message an oblique strategy."
	(interactive)
	(message (oblique-strategy)))

(defun my/reformat ()
  "Re-indent and refontify whole buffer."
	(interactive)
	(my/reindent)
	(font-lock-ensure))

(defun my/reindent ()
  "Indent the whole currently visited buffer."
	(interactive)
	(indent-region (point-min) (point-max)))

(defun my/set-face-show-paren-match-expression (&optional inverse-video)
  "Customises how to show paren matching, according to INVERSE-VIDEO."
  (interactive)
  (if inverse-video
    (set-face-attribute 'show-paren-match-expression nil
      ;; :inherit nil
      :inverse-video inverse-video)))

(defun my/theme-reset () "Disable loaded theme(s)."
  (interactive)
  (dolist (theme custom-enabled-themes) (disable-theme theme)))

(defun my/theme-load (theme &optional inverse-paren-expr)
  "Load the given THEME in parameter.  Optionally set our custom show-paren-match expression to use INVERSE-PAREN-EXPR."
  (interactive)
  (my/theme-reset)
  (if theme
    (load-theme theme t))
  (if inverse-paren-expr
    (my/set-face-show-paren-match-expression inverse-paren-expr)))

;; ======================================================================
;; Act IV
;; 1st party Emacs packages
;; ======================================================================

(use-package apropos
  :ensure nil
  :custom
  (apropos-do-all t "More extensive search"))

(use-package conf-mode
  :ensure nil
  :mode ("\\.cfg\\'" . conf-javaprop-mode)
  )

(use-package copyright
  :ensure nil
  :hook (before-save . copyright-update))

(use-package cus-edit
  :defer t
  :ensure nil
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode t))           ; Delete text when typing over selection

(use-package emacs
  :bind (
          ;; Disable annoying C-z
          ;; ("C-z"         . nil)

          ;; All my custom bindings starts with C-=, avalable in all encoutnered modes so far.
          ;; NB: C-= requires special config in the terminal emulator in TUI mode (see TUI comment below)

          ;; Edition related
          ("C-= /"     . comment-region)
          ("C-= \\"    . uncomment-region)
          ("C-= r i"   . my/reindent)
          ("C-= r f"   . my/reformat)

          ;; Dot files
          ("C-= . a"   . my/find-shell-aliases-file)
          ("C-= . b"   . my/find-bash-profile-file)
          ("C-= . e"   . my/find-emacs-init-file)
          ("C-= . f"   . my/find-shell-functions-file)
          ("C-= . h"   . my/find-hammerspoon-init-file)
          ("C-= . l"   . my/find-shell-local-script-file)
          ("C-= . z"   . my/find-zshrc-file)

          ;; Buffers
          ("C-= b s"   . my/buffer-new-scratch)
          ("C-= b k"   . my/buffer-kill-all-but-exceptions)
          ("C-= b r"   . recentf-open-files)

          ;; Themes
          ("C-= t d"   . (lambda () (interactive) (my/theme-load 'dichromacy t)))
          ("C-= t e"   . (lambda () (interactive) (my/theme-load 'seoul256 t)))
          ("C-= t m o" . (lambda () (interactive) (my/theme-load 'modus-operandi)))
          ("C-= t m v" . (lambda () (interactive) (my/theme-load 'modus-vivendi)))
          ("C-= t n"   . (lambda () (interactive) (my/theme-load nil)))
          ("C-= t s d" . (lambda () (interactive) (my/theme-load 'solarized-dark t)))
          ("C-= t s l" . (lambda () (interactive) (my/theme-load 'solarized-light t)))

          ;; Elisp Navigation
          ("C-= e f"   . find-function)
          ("C-= e k"   . find-function-on-key)
          ("C-= e l"   . find-library)
          ("C-= e v"   . find-variable)

          ;; Meta-shortcut to show all custom shortcuts
          ;; Note: it's the only one with a "C-= C" prefix, which is precisely unmatched
          ("C-= C-="   . (lambda () (interactive) (progn
                                                    (if (get-buffer "*Occur*")
                                                      (kill-buffer "*Occur*"))
                                                    (delete-other-windows)
                                                    (my/find-emacs-init-file)
                                                    ;; Occurences of "C-=" or "s-", excluding this and next line ;-)
                                                    (occur "\\(\"C-= [^C[\"]+\"\\)\\|\\(\"s-[^C[\"]+\"\\)")))))
  :config
  (set-fontset-font                   t nil "Monaco 12")
  (set-fontset-font                   t 'symbol (font-spec :family "Apple Color Emoji"))
  ;; (add-to-list                        'default-frame-alist '(fullscreen . maximized))
  (add-to-list                        'default-frame-alist '(font . "Roboto Mono 13"))
  (set-face-attribute 'default        nil :family "Monaco"    :height 120)
  (set-face-attribute 'fixed-pitch    nil :family "Monaco"    :height 120)
  (set-face-attribute 'variable-pitch nil :family "Helvetica Neue" :height 130)
  ;; (add-hook 'text-mode-hook (lambda () (variable-pitch-mode 1)))

  ;; Specific settings for emacs TUI and C-= handling
  (unless (display-graphic-p)
    ;; See https://stackoverflow.com/questions/10660060/how-do-i-bind-c-in-emacs
    (defun my/global-map-and-set-key (key command &optional prefix suffix)
      "Calls `my/map-key' KEY, then `global-set-key' KEY with COMMAND. PREFIX or SUFFIX can wrap the key when passing to `global-set-key'."
      (my/map-key key)
      (global-set-key (kbd (concat prefix key suffix)) command))

    (defun my/map-key (key)
      "Map KEY from escape sequence \"\e[emacs-KEY\."
      ;; It assumes that the underlying terminal has been configured to send the escape sequence \e[emacs-= when pressing C-=
      ;; For instance one can configure iTerm2 in iTerm2 > Preferences > Keys > Key Binding > +
      ;;  Keyboard shortcut: ^=
      ;;  Action:            Send keystrokes > Send Escape Sequence
      ;;  Esc+:              [emacs-C-=
      (define-key function-key-map (concat "\e[emacs-" key) (kbd key)))

    ;; Intercept \e[emacs-= to play C-=
    (my/map-key "C-="))

  ;; Specific settings for macOS
  (when (my/is-macos)
    (setq
      trash-directory            "~/.Trash/" ; Trash folder is ~/.Trash
      mac-option-modifier        'meta       ; Mac's left  Alt key (option) is Emacs's meta
      mac-right-option-modifier  'none       ; Mac's right Alt key (option) is macOS native
      mac-command-modifier       'super      ; Mac's command is macOS native
      )
    ;; Key bindings
    (global-set-key (kbd "s-<left>")  'previous-buffer)
    (global-set-key (kbd "s-<right>") 'next-buffer)
    (global-set-key (kbd "s-`")       'other-frame)
    (global-set-key (kbd "s--")       'text-scale-adjust)
    (global-set-key (kbd "s-+")       'text-scale-adjust)
    (global-set-key (kbd "s-0")       'text-scale-adjust)
    (global-set-key (kbd "s-a")       'mark-whole-buffer)
    (global-set-key (kbd "s-c")       'kill-ring-save)
    (global-set-key (kbd "s-k")       'kill-current-buffer)
    (global-set-key (kbd "s-l")       'goto-line)
    (global-set-key (kbd "s-m")       'iconify-frame)
    (global-set-key (kbd "s-n")       'make-frame)
    (global-set-key (kbd "s-o")       'other-window)
    (global-set-key (kbd "s-s")       'save-buffer)
    (global-set-key (kbd "s-x")       'kill-region)
    (global-set-key (kbd "s-v")       'yank)
    (global-set-key (kbd "s-w")       'delete-frame)
    (global-set-key (kbd "s-z")       'undo)

    ;; Make title bar transparent
    (add-to-list 'default-frame-alist'(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist'(ns-appearance . light)))

  :custom
  (cursor-type                   'hbar        "Uses the horizontal bar cursor." )
  (delete-by-moving-to-trash      t           "Uses OS's trash when deleting stuff.")
  (echo-keystrokes                0.5         "Shows keystrokes right away.")
  (frame-title-format
    (list
      '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
      " (" user-login-name "@" (system-name)  ")"
      ) "Sets the frame title.")
  (indicate-buffer-boundaries     "left"      "Buffer limits in the left fringe.")
  (indicate-empty-lines           t           "Empty lines in the left fringe.")
  (inhibit-startup-screen         t           "No splash screen.")
  (initial-scratch-message        nil         "No scratch message.")
  (tool-bar-mode                  nil         "Do not show toolbar.")
  (transient-mark-mode            t           "Highlight sactive region.")
  (visible-bell                   t           "Uses visible bell."))

(use-package files
  :ensure nil
  :config
  (defvar
    my/autosaves-dir
    (no-littering-expand-var-file-name "auto-save/")
    "My preferred auto-saves location.")
  (setq
    auto-save-file-name-transforms `((".*" ,my/autosaves-dir t))
    auto-save-list-file-prefix     my/autosaves-dir)
  :custom
  (auto-save-default              t           "Auto-save each file-visiting buffer.")
  (auto-save-interval             200         "Count of keystrokes before auto-save.")
  (auto-save-timeout              20          "Count of idle time (s) auto-save.")
  (backup-by-copying              t           "Use copying to create backup files.")
  (delete-old-versions            t           "Delete excess backup files silently.")
  (kept-new-versions              10          "Number of newest versions to keep when a new numbered backup is made. Includes the new backup.")
  (kept-old-versions              5           "Number of oldest versions to keep when a new numbered backup is made.")
  (make-backup-files              t           "Unconditional numeric backup versions.")
  (version-control                t           "Version numbers for backup files."))

(use-package font-core
  :ensure nil
  :config
  (global-font-lock-mode         1))          ; Toggle font Lock mode in all buffers

(use-package font-lock
  :ensure nil
  :custom
  (font-lock-maximum-decoration  t            "Uses maximum decoration."))

(use-package frame
  :ensure nil
  :custom
  (blink-cursor-blinks           0            "Cursor blinks for ever.")
  (blink-cursor-delay            0.2          "Minimal idle time before first blink.")
  (blink-cursor-interval         0.3          "Slightly quicker blink."))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode           t))          ; Enable global hl-line mode

(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes

  ;; Make headings larger in height relative to the main text.
  (setq modus-themes-scale-headings t)

  ;; Use a more prominent background color hl-line-mode
  (setq modus-themes-intense-hl-line t)

  :config
  (let
    ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f6f6f6"))) ;; "#d33682")))

(use-package mouse
  :ensure nil
  :custom
  (mouse-buffer-menu-mode-mult   20          "Longer mouse buffer menu (major mode)."))

(use-package org-mode
  :ensure nil
  :init
  ;; My org files
  (setq
    my/agendas-file "~/Org/Agendas.org"
    my/meta-file    "~/Org/Meta.org"
    my/todos-file   "~/Org/Todos.org")
  (defun my/show-org ()
    "Loads my org files, and set-up window accordingly"
    (interactive)
    (delete-other-windows)
    (find-file my/meta-file)
    (find-file my/todos-file)
    (split-window-right)
    (find-file-other-window my/agendas-file))
  :bind (
          ("C-= o a"   . (lambda () (interactive) (find-file my/agendas-file)))
          ("C-= o m"   . (lambda () (interactive) (find-file my/meta-file)))
          ("C-= o t"   . (lambda () (interactive) (find-file my/todos-file)))
          ("C-= o o"   . (lambda () (interactive) (my/show-org))))
  :mode ("\\.org\\'" . org-mode)
  :hook (
          ;; (org-mode . turn-on-auto-fill)
          ;; (org-mode    . org-num-mode)
          (org-mode    . visual-line-mode))
  :custom
  ;; Org-mode related
  (org-agenda-files                       (directory-files-recursively
                                            (file-name-directory "~/Org/MyTodos.org") "org$"))
  (org-descriptive-links                  nil                    "Do not decorate hyperlinks.")
  (org-link-frame-setup                   '((file . find-file)))
  (org-log-done                           t                      "Insert the DONE's timestamp.")
  (org-refile-targets                     '(
				                                     ;; (nil :maxlevel . 2)
				                                     (org-agenda-files
				                                       :maxlevel . 2))   "Specify targets for refile.")
  (org-refile-use-outline-path            'file                  "Include the file name (w/o directory) into path.")
  (org-refile-allow-creating-parent-nodes 'confirm               "Allow node creation upon refile.")
  (org-startup-indented                   'indet                 "Alternate stars and indent scheme.")

  ;; Org-babel related
  (org-babel-load-languages    '( (emacs-lisp . t)
                                  (shell . t)))
  (org-confirm-babel-evaluate  nil)
  (org-src-tab-acts-natively   t                  "Apply the indentation in source blocks"))

(use-package paren
  :ensure nil
  :defer 2
  :config
  (show-paren-mode               t)              ; Visualize matching parens
  (setq
    show-paren-delay             nil             ; Highlights without delay
    show-paren-style             'expression     ; Shows the matching expression
    ))

(use-package recentf
  :ensure nil
  :defer 2
  :commands (recentf-add-file recentf-apply-filename-handlers recentf-mode )
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
          (file-directory-p dired-directory)
          (not (string= "/" dired-directory)))
	    (let ((last-idx (1- (length dired-directory))))
        (recentf-add-file
          (if (= ?/ (aref dired-directory last-idx))
            (substring dired-directory 0 last-idx)
            dired-directory)))))
  :hook (dired-mode . recentf-add-dired-directory)
  :custom
  (recentf-max-menu-items        50              "How many recent files in menu?")
  (recentf-max-saved-items       50              "How many recent files to save?")
  :config
  (add-to-list                   'recentf-exclude no-littering-var-directory)
  (add-to-list                   'recentf-exclude no-littering-etc-directory)
  (recentf-mode                  t))             ; Enable recent files

(use-package saveplace
  :ensure nil
  :defer 2
  :unless noninteractive
  :config
  (save-place-mode               t))             ; Automatically save place.

(use-package scroll-bar
  :ensure nil
  :unless noninteractive
  :custom
  (scroll-bar-mode               "right"         "Use scroll bar on right"))

(use-package server
  :ensure nil
  :defer 2
  :custom
  (server-auth-dir
    ( ; Directory for server auth files
      expand-file-name "server-auth/" no-littering-var-directory)))

(use-package sgml-mode
  :ensure nil
  :defer 2
  :commands sgml-mode
  :custom
  (sgml-quick-keys               'close          "Auto close elements upon '</'."))

(use-package simple
  :ensure nil
  :custom
  (column-number-mode            t               "Displays column number.")
  (size-indication-mode          nil             "Show buffer size in mode line.")
  :hook
  ((text-mode . turn-on-auto-fill)))

(use-package tab-line
  :ensure nil
  :init)

(use-package vc-hooks
  :ensure nil
  :custom
  (vc-follow-symlinks            t               "Visit the real files.")
  (vc-make-backup-files          t               "Backup version controlled files."))

(use-package windmove
  :ensure nil
  :defer 2
  :config
  (windmove-default-keybindings))                ; Shifted arrow keys

;; ======================================================================
;; Act V
;; 3rd party Emacs packages
;; ======================================================================

(use-package all-the-icons)

(use-package auto-package-update
  :disabled                                         ; It raises some "Warning (package): Unnecessary call to ‘package-initialize’ in init file..."
  :init
  (setq                                             ; Override "last update day" file in no-littering var dir
    apu--last-update-day-filename
    (expand-file-name
      ".auto-package-update-last-package-update-day"
      no-littering-var-directory))
  :config
  (auto-package-update-maybe))

(use-package editorconfig
  :config
  (editorconfig-mode           t))              ; Enable EditorConfig

(use-package emmet-mode
  :hook (sgml-mode css-mode web-mode))

(use-package esup
  :commands esup
  :config
  (setq esup-user-init-file (file-truename "~/.emacs.d/init.el")))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :pin melpa-unstable
  :init (global-flycheck-mode)
  ;; Reminders, for Bash adn HTML files syntax checker:
  ;; `brew install shellcheck tidy-html5 markdownlint-cli`
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-status-emoji
  :init
  ;; Warning, on macOS, requires `(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji"))`
  (flycheck-status-emoji-mode))

(use-package flyspell
  :bind (
          ("C-= f m"   . flyspell-mode)
          ("C-= f p"   . flyspell-prog-mode)
          ("C-= f l"   . my/cycle-ispell-languages))
  :hook (
          (text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode))
  :config
  ;; Remap flyspell mouse buttons
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined)
  (define-key flyspell-mouse-map [down-mouse-2] nil)
  (define-key flyspell-mouse-map [mouse-2] nil)

  ;; Make sure DICPATH environment variables is there
  (setenv "DICPATH" (concat (getenv "HOME") "/Library/Spelling"))

  ;; On macOS, make sure DICPATH var env is set as well
  (when (my/is-macos)
    (setenv "DICTIONARY" "en_GB"))

  ;; Find aspell and hunspell automatically
  (cond
    ;; Try Hunspell at first; if hunspell does NOT exist, use aspell
    ((executable-find "hunspell")
      (setq
        ispell-program-name "hunspell"
        ispell-local-dictionary "en_GB"
        ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_GB")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_GB,en_GB-med")` to check with multiple dictionaries
        '(("en_GB"  "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)
           ("fr-moderne" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "fr-moderne") nil utf-8))))
    ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
      (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB"))))

  ;; Make appear the language in modeline, and change spelling language dynamically
  ;; Source: Manuel Uberti at https://emacs.stackexchange.com/a/48978
  (setq ispell-dictionary "en_GB")
  (defun my/current-dictionary-mode-line (language)
    "Return the current dictionary from LANGUAGE for the mode line."
    (interactive)
    ;; (let ((dict (substring language 0 2)))
    ;; (concat " " dict)))
    (concat " " language))
  (defvar my/languages-ring nil "Languages ring for Ispell")
  (let ((languages '("fr-moderne" "en_GB")))
    (setq my/languages-ring (make-ring (length languages)))
    (dolist (elem languages) (ring-insert my/languages-ring elem)))
  (defun my/cycle-ispell-languages ()
    "Cycle ispell languages in `my/languages-ring'. Change dictionary and mode-line lighter accordingly."
    (interactive)
    (let ((language (ring-ref my/languages-ring -1)))
      (ring-insert my/languages-ring language)
      (ispell-change-dictionary language)
      (setq flyspell-mode-line-string (my/current-dictionary-mode-line language))
      (force-mode-line-update)))
  (setq flyspell-mode-line-string (my/current-dictionary-mode-line ispell-dictionary)))

(use-package free-keys
  :custom
  (free-keys-modifiers  '("" "C" "M" "s" "C-M" "C-s" "M-s")))

(use-package groovy-mode
  :mode ("\\.groovy\\'" . groovy-mode))

(use-package iedit
  :custom
  (iedit-toggle-key-default (kbd "C-S-s"))
  )

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

(use-package lua-mode
  :pin melpa-unstable
  :defer 1)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package markdown-mode
  :mode ( ("\\.md\\'"          . markdown-mode)
          ("\\.markdown\\'"    . markdown-mode)))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-slant-function #'moody-slant-apple-rgb)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package oblique
  ;; :defer 5
  :ensure nil ;; Loaded locally
  :config
  (add-hook 'after-init-hook
    (lambda ()
      (setq initial-scratch-message (oblique-strategy))))
  ;; Does not seem to work. Use instead:
  ;; https://github.com/tecosaur/emacs-config/blob/master/config.org#splash-screen
  :bind (
          ("C-= x o"   . oblique-strategy)
          ("C-= x O"   . insert-oblique-strategy))
  :custom
  (oblique-edition "strategies/oblique-strategies-condensed.txt" "Version to draw a strategy from."))

(use-package org-bullets
  :commands (org-bullets-mode)
  :init
  (add-hook 'org-mode-hook
	  (lambda ()
	    (org-bullets-mode t))))

(use-package ox-hugo
  :after ox)

(use-package pandoc-mode
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  ;; (add-hook 'org-mode-hook 'pandoc-mode)
  )

(use-package powerline
  :config
  ;; Use Powerline to make tabs nicer
  (defvar my/tab-height 22 "Height of my custom tabs.")
  (defvar my/tab-left   (powerline-wave-right 'tab-line nil my/tab-height) "Left character to use in displayed tab name.")
  (defvar my/tab-right  (powerline-wave-left nil 'tab-line my/tab-height) "Right character to use in displayed tab name.")
  (defun my/tab-line-tab-name-buffer (buffer &optional _buffers)
    (powerline-render (list my/tab-left
                        (format " %s  " (buffer-name buffer))
                        my/tab-right)))
  (setq tab-line-tab-name-function #'my/tab-line-tab-name-buffer)
  (setq tab-line-new-button-show nil)
  (setq tab-line-close-button-show nil)
  )

(use-package seoul256-theme
  :pin melpa-unstable
  :config
  (setq seoul256-background 253)
  )

(use-package solarized-theme
  :config
  (my/theme-load 'solarized-light))

(use-package treemacs
  :defer 2
  :bind (
          ("C-= b t"   . treemacs))
  :custom
  (treemacs-space-between-root-nodes nil "No space between root nodes.")
  :custom-face
  (treemacs-file-face      ((t (:height 1.0))))
  (treemacs-directory-face ((t (:height 1.0))))
  (treemacs-root-face      ((t (:height 1.0)))))

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package try)

(use-package web-mode
  :mode
  ("\\.ejs\\'" . web-mode)
  ("\\.html\\'" . web-mode))

(use-package which-key
  :defer 5
  :commands which-key-mode
  :config
  (which-key-mode))

;;; init.el ends here
