;;; init.el --- Emmanuel Roubion's personal init file -*- lexical-binding: t; coding: utf-8  -*-

;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021, 2022 Emmanuel Roubion

;; Author: Emmanuel Roubion
;; Keywords: Emacs
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
;; (or something similar to)
;;   open -n /usr/local/Cellar/emacs-mac/emacs-28.1-mac-9.0/Emacs.app --args -q --eval='(message "%s" (emacs-init-time))'
;; On other OSes:
;;   emacs -q --eval='(message "%s" (emacs-init-time))'

;; Currently, startup time in graphical macOS environment is:
;;   <even before>: 1.86 seconds, with 5 Cs
;;   2020-09:       2.49 seconds, with 11 GCs
;;   2021-01:       5.26 seconds, with 11 GCs
;;   2021-03:       1.15 seconds, with 14 GCs (on Apple Silicon M1)
;;   2021-10:       1.36 seconds, with 13 GCs (on Apple Silicon M1)
;;   2022-05:       2.17 seconds, with 15 garbage collections (On 2 GHz Intel Core i5 quadcore)

;; Avoid GC during Emacs startup. Garbage collection when Emacs loses focus.
;; Inspiration from https://github.com/narendraj9/dot-emacs/blob/master/init.el
(setq
  ;; The default is 800 kilobytes. Measured in bytes.
  gc-cons-threshold (* 50 1000 1000)

  ;; Portion of heap used for allocation. Defaults to 0.1.
  gc-cons-percentage 0.6)

;; Restore good defaults post startup
(add-hook 'after-init-hook
  (lambda () (setq
               gc-cons-threshold (* 2 1000 1000)
               gc-cons-percentage 0.1)))


;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
	(lambda ()
	  (message "Emacs ready in %s with %d garbage collections."
		  (format "%.2f seconds"
			  (float-time
			    (time-subtract after-init-time before-init-time)))
		  gcs-done)))

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

;; A few use-package settings
(if init-file-debug
  (setq
    use-package-verbose            t
    use-package-expand-minimally   nil
    use-package-compute-statistics t
    debug-on-error                 t)
  (setq
    use-package-verbose            nil
    use-package-expand-minimally   t))

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

(defun my/log (text) "Message the given TEXT in *Messages* with a custom, timestamped, prefix."
  (message (format "[%S] %S" (format-time-string "%Y/%m/%d %H:%M:%S:%3N %z") text)))

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

(defun my/set-face-show-paren-match-expression (&optional is-inverse-video)
  "Customises how to show paren matching, according to IS-INVERSE-VIDEO."
  (interactive)
  (if is-inverse-video
    (progn
      (set-face-attribute 'show-paren-match-expression nil
			  ;; :inherit nil
			  :inverse-video is-inverse-video)
      (my/log (format "Set face attribute 'show-paren-match-expression' to %S." is-inverse-video)))))

(defun my/theme-reset () "Disable loaded theme(s)."
  (interactive)
  (dolist (theme custom-enabled-themes) (disable-theme theme)))

(defun my/theme-load (theme &optional inverse-paren-expr)
  "Load the given THEME in parameter.  Optionally set our custom show-paren-match expression to use INVERSE-PAREN-EXPR."
  (interactive)
  (my/log "Nullified theme.")
  (my/theme-reset)
  (if theme
    (progn
      (load-theme theme t)
      (my/log (format "Loaded theme %S." theme))))
  (if inverse-paren-expr
    (my/set-face-show-paren-match-expression inverse-paren-expr)))

;; Adapted from https://www.emacswiki.org/emacs/UnfillRegion
(defun my/unfill-region (beg end)
  "Unfill the region from BEG to END, joining text paragraphs into a single logical line."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

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
  :functions my/buffer-kill-all-exceptions my/map-key
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
          ("C-= r u"   . my/unfill-region)

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

          ;; Org capture
          ("C-= C-c C-c"  . org-capture)

          ;; Modes
          ("C-= m c"   . css-mode)
          ("C-= m f"   . fundamental-mode)
          ("C-= m g"   . groovy-mode)
          ("C-= m h"   . html-mode)
          ("C-= m j 2" . js2-mode)
          ("C-= m j a" . java-mode)
          ("C-= m j s" . json-mode)
          ("C-= m l i" . lisp-interaction-mode)
          ("C-= m l u" . lua-mode)
          ("C-= m l m" . lisp-mode)
          ("C-= m m"   . markdown-mode)
          ("C-= m o"   . org-mode)
          ("C-= m O"   . org-bullets-mode)
          ("C-= m p"   . pandoc-mode)
          ("C-= m s"   . sgml-mode)
          ("C-= m t"   . text-mode)
          ("C-= m x"   . xml-mode)
          ("C-= m w"   . web-mode)
          ("C-= m W"   . whitespace-mode)

          ;; Themes
          ;; ("C-= t d"   . (lambda () (interactive) (my/theme-load 'dichromacy t)))
          ("C-= t d d" . (lambda () (interactive) (my/theme-load 'doom-one t)))
          ("C-= t d l" . (lambda () (interactive) (my/theme-load 'doom-one-light t)))
          ("C-= t j"   . (lambda () (interactive) (my/theme-load 'solo-jazz t)))
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
          ("C-= C-="   . (lambda ()
                           (interactive)
                           (progn
                             (if (get-buffer "*Occur*")
                               (kill-buffer "*Occur*"))
                             (delete-other-windows)
                             (my/find-emacs-init-file)
                             ;; Occurences of "C-=" or "s-", excluding this and next line ;-)
                             (occur "\\(\"C-= [^C[\"]+\"\\)\\|\\(\"s-[^C[\"]+\"\\)")))))
  :init
  ;; Thanks to https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; Man pages are rendered properly irrespective of LC_* variable values.
  (setq locale-coding-system 'utf-8)
  (setq buffer-file-coding-system 'utf-8)

  :config
  (set-fontset-font                   t nil     "Fira Code 14")
  (set-fontset-font                   t 'symbol (font-spec :family "Apple Color Emoji"))
  ;; (add-to-list                     'default-frame-alist '(fullscreen . maximized))
  (add-to-list                        'default-frame-alist '(font . "Fira Code 14"))
  (set-face-attribute 'default        nil :family "Fira Code"          :height 140)
  (set-face-attribute 'fixed-pitch    nil :family "Fira Code"          :height 140)
  (set-face-attribute 'variable-pitch nil :family "Georgia" :height 170)

  ;; Specific settings for emacs TUI and C-= handling
  (unless (display-graphic-p)
    ;; See https://stackoverflow.com/questions/10660060/how-do-i-bind-c-in-emacs
    (defun my/map-key (key)
      "Map KEY from escape sequence \"\e[emacs-KEY\."
      ;; It assumes that the underlying terminal has been configured to send the escape sequence \e[emacs-= when pressing C-=
      ;; For instance one can configure iTerm2 in iTerm2 > Preferences > Keys > Key Binding > +
      ;;  Keyboard shortcut: ^=
      ;;  Action:            Send keystrokes > Send Escape Sequence
      ;;  Esc+:              [emacs-C-=
      (define-key function-key-map (concat "\e[emacs-" key) (kbd key)))

    (defun my/global-map-and-set-key (key command &optional prefix suffix)
      "Calls `my/map-key' KEY, then `global-set-key' KEY with COMMAND. PREFIX or SUFFIX can wrap the key when passing to `global-set-key'."
      (my/map-key key)
      (global-set-key (kbd (concat prefix key suffix)) command))

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
    (global-set-key (kbd "s-<left>")  'move-beginning-of-line) ;; was 'previous-buffer, but too conflicting with macOS defaults
    (global-set-key (kbd "s-<right>") 'move-end-of-line)       ;; was 'next-buffer, but same as above
    (global-set-key (kbd "s-<up>")    'beginning-of-buffer)
    (global-set-key (kbd "s-<down>")  'end-of-buffer)
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
  ;; (visible-bell                   t           "Uses visible bell."))
  )

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

(use-package mouse
  :ensure nil
  :custom
  (mouse-buffer-menu-mode-mult   20          "Longer mouse buffer menu (major mode)."))

(use-package org-capture
  :after org
  :ensure nil)

(use-package org
  :ensure nil
  :functions my/show-org
  :init
  ;; My org files
  ;;(defvar my/agendas-file "~/Org/Agendas.org" "This variable points to my Agenda.org file.")
  ;;(defvar my/meta-file    "~/Org/Meta.org"    "This variable points to my Meta.org file.")
  ;;(defvar my/todos-file   "~/Org/Todos.org"   "This variable points to my Todos.org file.")
  (defun my/show-org ()
    "Loads my org files, and set-up window accordingly"
    (interactive)
    (delete-other-windows)
    (find-file my/meta-file)
    (find-file my/todos-file)
    (split-window-right)
    (find-file-other-window my/agendas-file))
  ;; Use a nice readable proportional fonts
  (let* ((variable-tuple
           (cond
             ;; Preferred
             ((x-list-fonts "Georgia")         '(:font "Georgia"))
             ;; Sans-serif
             ;;((x-list-fonts "SF Pro Text")     '(:font "SF Pro Text"))
             ;;((x-list-fonts "SF Pro Icons")    '(:font "SF Pro Icons"))
             ((x-list-fonts "Helvetica Neue")  '(:font "Helvetica Neue"))
             ((x-list-fonts "Helvetica")       '(:font "Helvetica"))
             ((x-list-fonts "Arial")           '(:font "Arial"))
             ((x-list-fonts "sans-serif")      '(:font "sans-serif"))
             ;; Other serif, just in case :-)
             ((x-list-fonts "charter")         '(:font "charter"))
             ((x-list-fonts "Cambria")         '(:font "Cambria"))
             ((x-list-fonts "Times New Roman") '(:font "Times New Roman"))
             ((x-list-fonts "Times")           '(:font "Times"))
             (nil (warn "Cannot find an elegant font: install one please")) ;; Should not happen!
             ))
          (headline           `(:inherit default ))) ;;:weight bold)))
    ;; Adapt headings size
    (custom-theme-set-faces
      'user
      `(org-level-1               ((t (,@headline ,@variable-tuple :height 1.05))))
      `(org-level-2               ((t (,@headline ,@variable-tuple :height 1.05))))
      `(org-level-3               ((t (,@headline ,@variable-tuple :height 1.05))))
      `(org-level-4               ((t (,@headline ,@variable-tuple :height 1.05))))
      `(org-level-5               ((t (,@headline ,@variable-tuple))))
      `(org-level-6               ((t (,@headline ,@variable-tuple))))
      `(org-level-7               ((t (,@headline ,@variable-tuple))))
      `(org-level-8               ((t (,@headline ,@variable-tuple))))
      `(org-block                 ((t (:inherit fixed-pitch))))
      `(org-code                  ((t (:inherit (shadow fixed-pitch)))))
      `(org-document-info         ((t (:foreground "dark orange"))))
      `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
      `(org-document-title        ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
      ;; `(org-headline-done         ((t (,@headline ,@variable-tuple :strike-through t))))
      `(org-indent                ((t (:inherit (org-hide fixed-pitch)))))
      `(org-link                  ((t (:foreground "royal blue" :underline t))))
      `(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
      `(org-property-value        ((t (:inherit fixed-pitch))) t)
      `(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
      `(org-table                 ((t (:inherit fixed-pitch :foreground "#83a598"))))
      `(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
      `(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))))
  :bind (
          ("C-= o a"  . (lambda () (interactive) (find-file my/agendas-file)))
          ("C-= o m"  . (lambda () (interactive) (find-file my/meta-file)))
          ("C-= o t"  . (lambda () (interactive) (find-file my/todos-file)))
          ("C-= o o"  . (lambda () (interactive) (my/show-org))))
  :mode ("\\.org\\'"  . org-mode)
  :hook (
          ;; (org-mode    . org-num-mode)        ;; Best with (org-bullets-bullet-list '("\u200b")
          (org-mode    . variable-pitch-mode)
          (org-mode    . visual-line-mode))
  :custom
  ; (org-agenda-files                       (directory-files-recursively
  ;                                           (file-name-directory "~/Org/MyTodos.org") "org$"))
  (org-blank-before-new-entry             '(
                                             (heading . nil)
                                             (plain-list-item . nil))
                                                                "Removes gap when you add a new heading.")


  (org-catch-invisible-edits              'error)
  (org-descriptive-links                  nil                    "Do not decorate hyperlinks.")
  (org-ellipsis                           " ⤵"                  "Use this ellipsis string.")
  (org-hide-emphasis-markers              t                      "Hide emphasis markup")
  (org-link-frame-setup                   '((file . find-file)))
  (org-list-demote-modify-bullet          '(
                                             ("-" . "+")
                                             ("+" . "*")
                                             ("*" . "-")))
  (org-log-done                           t                      "Insert the DONE's timestamp.")
  (org-refile-targets                     '(
				                                     ;; (nil :maxlevel . 2)
				                                     (org-agenda-files
				                                       :maxlevel . 2))   "Specify targets for refile.")
  (org-refile-use-outline-path            'file                  "Include the file name (w/o directory) into path.")
  (org-refile-allow-creating-parent-nodes 'confirm               "Allow node creation upon refile.")
  (org-src-tab-acts-natively              t                      "TAB in a code block follows the language major mode buffer.")
  (org-startup-indented                   'indet                 "Alternate stars and indent scheme.")

  ;; Org-babel related
  (org-babel-load-languages               '(
                                             (emacs-lisp . t)
                                             (shell . t)))
  (org-confirm-babel-evaluate             nil)
  (org-src-tab-acts-natively              t                      "Apply the indentation in source blocks")
  (org-capture-templates
    '(("h" "Hugo post"
        entry (file+olp "~/www/all-posts.org" "Posts")
        (function org-hugo-new-subtree-post-capture-template))
       )))

(use-package org-indent
  :after org
  :ensure nil)

(use-package org-num
  :after org
  :ensure nil)

(use-package paren
  :defer 1
  :ensure nil
  :config
  (show-paren-mode               t)              ; Visualize matching parens
  (setq
    show-paren-delay             nil             ; Highlights without delay
    show-paren-style             'expression     ; Shows the matching expression
    ))

(use-package recentf
  :commands (recentf-add-file recentf-apply-filename-handlers recentf-mode )
  :ensure nil
  :defer 1
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
  (recentf-auto-cleanup          'never          "Don't try to clean-up the files list")
  (recentf-max-menu-items        50              "How many recent files in menu?")
  (recentf-max-saved-items       50              "How many recent files to save?")
  :config
  (add-to-list                   'recentf-exclude no-littering-var-directory)
  (add-to-list                   'recentf-exclude no-littering-etc-directory)
  :init
  (recentf-mode                  t))             ; Enable recent files

(use-package saveplace
  :defer 2
  :ensure nil
  :unless noninteractive
  :config
  (save-place-mode               t))             ; Automatically save place.

(use-package scroll-bar
  :ensure nil
  :unless noninteractive
  :custom
  (scroll-bar-mode               "right"         "Use scroll bar on right"))

(use-package server
  :defer 2
  :ensure nil
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
  (size-indication-mode          nil             "Show buffer size in mode line."))

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
;; Do a manual 'all-the-icons-install-fonts' at fresh install

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

(use-package deft
  :bind ("C-= d"   . deft)
  :commands (deft)
  :custom
  (deft-default-extension             "org")
  (deft-directory                      "~/Deft")
  (deft-extensions                     '("org" "md" "txt"))
  (deft-recursive                      t)
  (deft-use-filter-string-for-filename t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-minor-modes t))

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
  :commands   flyspell-correct-word
  :defer 10
  :functions flyspell-correct-word my/current-dictionary-mode-line ring-insert ring-ref
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
        '(
           ("en_GB"      "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB")      nil utf-8)
           ("fr-moderne" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "fr-moderne") nil utf-8))
        ))
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
    ;; (concat " " language)
    (concat " "
      (cond
        ((string=  language "en_GB") "🇬🇧")
        ((string=  language "fr-moderne") "🇫🇷")
	      ("🏳"))))
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

(use-package oblique
  :defer 10
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

;; (use-package org-bullets
;;   :hook
;;   (org-mode . org-bullets-mode)
;;   :custom
;;   ;; (org-bullets-bullet-list '("◉" "○" "●" "►" "•"))
;;   (org-bullets-bullet-list '("\u200b"))) ;; Zero width space, best used with (org-num-mode)

(use-package org-superstar
  :after org
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t)                ; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("RM" . 9744)
                                          ("SOMEDAY" . 9744)))
  :hook (org-mode . org-superstar-mode))

(use-package ox-jira
  :after ox
  :pin melpa-unstable)

(use-package ox-hugo
  :after ox
  :pin melpa-unstable
  :commands (org-hugo-new-subtree-post-capture-template)
  :init
  ;; Define a template for Hugo posts
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post. See `org-capture-templates' for more information."
    (let* (
            (title (read-from-minibuffer "Post Title: ")) ; Prompt to enter the post title
            (fname (org-hugo-slug title)))                ; Slug it
      (mapconcat #'identity
		    `(
		       ,(concat "* IDEA " title)
		       ":PROPERTIES:"
		       ,(concat ":export_file_name: " fname)
		       ":END:"
		       "%?\n")                                        ; Place the cursor here finally
		    "\n"))))

(use-package pandoc-mode
  :config
  ;; (add-hook 'org-mode-hook 'pandoc-mode)
  (add-hook 'markdown-mode-hook 'pandoc-mode))

;; More info found here:
;;  - https://docs.microsoft.com/en-us/archive/blogs/dotnetinterop/run-powershell-as-a-shell-within-emacs
;;  - https://www.reddit.com/r/emacs/comments/m3cx27/powershellwindows_terminal_from_emacs/
(use-package powershell
  :pin melpa-unstable
  :mode
  ("\\.ps1\\'" . powershell-mode))

(use-package string-inflection
  :bind (
          ("C-= s i c C" . string-inflection-camelcase)
          ("C-= s i c c" . string-inflection-lower-camelcase)
          ("C-= s i u"   . string-inflection-underscore)
          ("C-= s i U"   . string-inflection-capital-underscore)
          ("C-= s i k"   . string-inflection-kebab-case)))

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

;; ======================================================================
;; Act VI
;; 3rd party Emacs themes
;; ======================================================================

(use-package doom-themes
  :config
  (doom-themes-visual-bell-config))

(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes, if any

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
    (set-face-attribute 'mode-line-inactive nil :background "#f6f6f6")) ;; "#d33682"))
  ;; (my/theme-load 'modus-operandi t)
  )

(use-package seoul256-theme
  :pin melpa-unstable
  :config
  (setq seoul256-background 253))

(use-package solarized-theme)

(use-package solo-jazz-theme
  :config
  (my/theme-load 'solo-jazz nil))

;; Key bindings reminders:
;; C-u C-SPC    Move cursor to previous marked position in current buffer.
;;              Repeat call will move cursor to positions in mark-ring.
;; C-x 8 ' e    Insert a é
;; C-x 8 C-h    List all available character compositions type
;; C-x 8 ' C-h  List of all accented characters, and so on.

;;; init.el ends here
