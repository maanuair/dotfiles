;;; init.el --- Emmanuel Roubion's personal init file -*- lexical-binding: t; -*-

;; Copyright © 2016, 2017, 2018, 2019, 2020 Emmanuel Roubion

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

;; My default, naked, Emacs startup time is ~ 0.4 seconds in UI mode.
;; On macOS, run the following to discover it:
;;   open -n /Applications/Emacs.app --args -q --eval='(message "%s" (emacs-init-time))'
;; On other OSes:
;;   emacs -q --eval='(message "%s" (emacs-init-time))'

;; Currently, startup time is:
;;   past:         1.86 seconds, with 5 GCs
;;   2020-09 past: 2.49 seconds, with 11 GCs

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

;; Add no-littering Git sub-module in load-path
(add-to-list 'load-path (expand-file-name "no-littering/" my/elisp-dir))

;; Add oblique strategies Git sub-module in load-path
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
  (require 'use-package))

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
              (and
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

(defun my/buffer-reformat ()
  "Re-indent and refontify whole buffer."
	(interactive)
	(my/buffer-reindent)
	(font-lock-ensure))

(defun my/buffer-reindent ()
  "Indent the whole currently visited buffer."
	(interactive)
	(indent-region (point-min) (point-max)))

(defun my/find-init-file () "Visit my Emacs initialization file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun my/find-profile-file () "Visit my Bash profile file."
  (interactive)
  (find-file "~/.profile"))

(defun my/find-profile-local-file () "Visit my Bash local profile."
  (interactive)
  (find-file "~/.profile.local"))

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

(defun my/set-face-show-paren-match-expression (&optional inverse-video)
  "Customises how to show paren matching, according to INVERSE-VIDEO."
  (interactive)
  (if inverse-video
    (set-face-attribute 'show-paren-match-expression nil
      ;; :inherit nil
      :inverse-video inverse-video)))

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
  :bind ( ("C-z"         . nil)        ; Disable C-z
          ("C-c C-. i"   . my/find-init-file)
          ("C-c C-. p"   . my/find-profile-file)
          ("C-c C-. l"   . my/find-profile-local-file)
          ("C-c C-/"     . comment-region)
          ("C-c C-\\"    . uncomment-region)
          ("C-c C-b i"   . my/buffer-reindent)
          ("C-c C-b f"   . my/buffer-reformat)
          ("C-c C-b s"   . my/buffer-new-scratch)
          ("C-c C-b k"   . my/buffer-kill-all-but-exceptions)
          ("C-c C-l d"   . (lambda () (interactive) (my/theme-load 'dichromacy t)))
          ("C-c C-l e"   . (lambda () (interactive) (my/theme-load 'seoul256 t)))
          ("C-c C-l s d" . (lambda () (interactive) (my/theme-load 'solarized-dark t)))
          ("C-c C-l s l" . (lambda () (interactive) (my/theme-load 'solarized-light t)))
          ("C-c C-l n"   . (lambda () (interactive) (my/theme-load nil)))
          ("C-c C-l o"   . (lambda () (interactive) (my/theme-load 'modus-operandi)))
          ("C-c C-l v"   . (lambda () (interactive) (my/theme-load 'modus-vivendi)))
          ("C-c C-e f"   . find-function)
          ("C-c C-e k"   . find-function-on-key)
          ("C-c C-e l"   . find-library)
          ("C-c C-e v"   . find-variable)
          ("M-g"         . goto-line)
          ("<f6>"        . recompile)
          ("<f6>"        . next-error)
          ("<f12>"       . auto-revert-tail-mode))
  :config
  (set-fontset-font t nil "Roboto Mono 13")
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji"))
  (add-to-list 'default-frame-alist '(font . "Roboto Mono 13"))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Specific settings for macOS
  (when (my/is-macos)
    (setq
      trash-directory             "~/.Trash/" ; Trash folder is ~/.Trash
      mac-right-option-modifier   'none       ; Make the right Alt key (option) native
      ))

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
  (defvar my/autosaves-dir        (no-littering-expand-var-file-name "auto-save/") "My preferred auto-saves location.")
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

(use-package modus-operandi-theme
  :config
  (my/theme-load 'modus-operandi)
  (let
    ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f6f6f6"))) ;; "#d33682")))

(use-package modus-vivendi-theme)

(use-package mouse
  :ensure nil
  :custom
  (mouse-buffer-menu-mode-mult   20          "Longer mouse buffer menu (major mode)."))

(use-package org-mode
  :ensure nil
  :init
  (setq
    my/agendas-file "~/Org/Agenda.org"
    my/meta-file    "~/Org/Meta.org"
    my/todos-file   "~/Org/Todos.org")
  :bind (
	        ("C-c C-o a" . (lambda () (interactive) (find-file my/agendas-file)))
          ("C-c C-o m" . (lambda () (interactive) (find-file my/meta-file)))
          ("C-c C-o t" . (lambda () (interactive) (find-file my/todos-file)))
	        ("C-c C-o *" . (lambda () (interactive) (progn
	                                                  (find-file my/agendas-file)
	                                                  (find-file my/meta-file)
	                                                  (find-file my/todos-file)))))
  :mode ("\\.org\\'" . org-mode)
  :hook
  ((org-mode . turn-on-auto-fill))
  :custom
  ;; Org-mode related
  (org-agenda-files            (directory-files-recursively (file-name-directory "~/Org/MyTodos.org") "org$"))
  (org-descriptive-links       nil                 "Do not decorate hyperlinks.")
  (org-log-done                t                   "Insert the DONE's timestamp.")
  (org-refile-targets          '(
				 ;; (nil :maxlevel . 2)
				 (org-agenda-files
				  :maxlevel . 2)) "Specify targets for refile.")
  (org-refile-use-outline-path 'file              "Include the file name (without directory) into the path.")
  (org-startup-indented        'indet             "Alternate stars and indent scheme.")

  ;; Org-babel related
  (org-babel-load-languages    '(
				 (emacs-lisp . t)
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
    )
  )

(use-package recentf
  :ensure nil
  :bind (
          ("C-c C-r" . recentf-open-files))
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

(use-package csound-mode
  :mode ( ("\\.csd\\'" . csound-mode)
  	      ("\\.orc\\'" . csound-mode)
  	      ("\\.sco\\'" . csound-mode)
  	      ("\\.udo\\'" . csound-mode)))

(use-package dashboard
  :disabled
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-items
    '(
       (recents   . 15)
       (bookmarks . 5)
       (registers . 5))
		"Set the items to put at startup" )
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons    t)
  (dashboard-set-navigator     t)
  (dashboard-startup-banner    'logo))

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
  :init (flycheck-status-emoji-mode)) ;; Warning, on macOS, requires `(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji"))`

(use-package flyspell
  :bind (
          ("C-c C-s s" . flyspell-mode)
          ("C-c C-s S" . flyspell-prog-mode)
          ("C-c C-s d" . my/cycle-ispell-languages))
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
        ispell-program-name "/usr/local/bin/hunspell"
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

(use-package groovy-mode
  :mode ("\\.groovy\\'" . groovy-mode))

(use-package iedit
  :custom
  (iedit-toggle-key-default (kbd "C-S-s"))
  )

(use-package indium
  :bind ("C-x c" . indium-connect)
  :init
  (setq
    indium-chrome-data-dir
    (expand-file-name "indium-chrome-profile" no-littering-var-directory))
  (make-directory indium-chrome-data-dir t))

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
          ("C-c M-o"  . my/oblique-strategy)
          ("C-c M-O" . insert-oblique-strategy))
  :custom
  (oblique-edition "strategies/oblique-strategies-condensed.txt" "Version to draw a strategy from."))

(use-package org-bullets
  :commands (org-bullets-mode)
  :init
  (add-hook 'org-mode-hook
	  (lambda ()
	    (org-bullets-mode t))))

(use-package org-roam
  :pin melpa-unstable
  :custom
  (org-roam-directory "~/Org-Roam"))

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

(use-package restclient
  :disabled
  :mode ("\\.rest\\'" . restclient-mode))

(use-package seoul256-theme
  :pin melpa-unstable
  :config
  (setq seoul256-background 253)
  ;; (my/theme-load 'seoul256 t)
  )

(use-package solarized-theme)

(use-package svelte-mode
  :pin melpa-unstable)

(use-package treemacs
  :defer 2
  :bind ("C-C t" . treemacs)
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
