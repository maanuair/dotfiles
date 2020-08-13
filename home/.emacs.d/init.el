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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup time performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; My default, naked, Emacs startup time is ~ 0.3 seconds in UI mode.
;; On macOS, run the following to discover it:
;;   open -n /Applications/Emacs.app --args -q --eval='(message "%s" (emacs-init-time))'
;; On other OSes:
;;   emacs -q --eval='(message "%s" (emacs-init-time))'

;; Currently, startup time is: 1.86 seconds, with 5 GCs

;; Make startup faster by reducing the frequency of GC.
(setq
  ;; The default is 800 kilobytes. Measured in bytes.
  gc-cons-threshold (* 50 1000 1000)

  ;; Portion of heap used for allocation. Defaults to 0.1.
  gc-cons-percentage 0.6)

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
       gc-cons-threshold 800000
       gc-cons-percentage 0.1)
     (garbage-collect)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up the load path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define my-elisp-dir local folder, and add it in load path
(setq my-elisp-dir (expand-file-name "my-elisp/" user-emacs-directory))
(add-to-list 'load-path my-elisp-dir)

;; Add no-littering Git sub-module in load-path
(add-to-list 'load-path (expand-file-name "no-littering/" my-elisp-dir))

;; Reminder about Git submodules option, when cloning this project
;;   git clone --recurse-submodules git@github.com:maanuair/dotfiles.git
;;
;; When --recurse-submodules is forgotten, use:
;;   git submodule init && git submodule update
;;
;; Or simply:
;;   git submodule update --init

;; Bootstrap no-littering.el
(setq
  my-no-littering-dir         (expand-file-name "../.emacs.runtime-dirs/" user-emacs-directory)
  no-littering-etc-directory  (expand-file-name "etc/" my-no-littering-dir)
  no-littering-var-directory  (expand-file-name "var/" my-no-littering-dir))
(require 'no-littering)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(setq
  package-enable-at-startup        nil                ; Prevent initialising twice
  package-user-dir                 (                  ; Prefered location of packages
                                     expand-file-name
                                     "packages/" my-no-littering-dir))

;; Initialize package.el
(unless package--initialized (package-initialize nil))
;;(package-initialize)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1st party Emacs packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package apropos
  :ensure nil
  :custom
  (apropos-do-all t "More extensive search"))

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
  :bind ( ("C-z"     . nil)
          ("C-c i"   .                 ; Visit init file
            (lambda ()
              (interactive)
              (find-file (expand-file-name "init.el" user-emacs-directory))))
          ("C-c l"   . my-load-theme-light)
          ("C-c d"   . my-load-theme-dark)
          ("M-g"     . goto-line)
          ("M-o"     . other-window)
          ("<f5>"    . my-reformat)
          ("<f6>"    . recompile)
          ("S-<f6>"  . next-error)
          ("S-<f12>" . auto-revert-tail-mode))
  :config
  (defun is-macOs () "Return t when the system is a macOS"
	  (interactive)
	  (equal system-type 'darwin))
  (defun is-win32 () "Return t when the system is a Windows"
	  (interactive)
	  (equal system-type 'windows-nt))
  (defun my-indent-buffer () "Indent the whole currently visited buffer."
	  (interactive)
	  (indent-region (point-min) (point-max)))
  (defun my-reformat () "Re-indent and refontify whole buffer."
	  (interactive)
	  (my-indent-buffer)
	  (font-lock-fontify-buffer))
  ;; Make sure the default frame size is maximized
  (add-to-list
    'default-frame-alist
    '(fullscreen . maximized))
  ;; Specific settings for macOS
  (when (is-macOs)
    (setq
      trash-directory             "~/.Trash/" ; Trash folder is ~/.Trash
      mac-right-option-modifier   'none       ; Make the right Alt key (option) native
      )
    )
  :custom
  (delete-by-moving-to-trash      t           "Uses system's trash when deleting stuff.")
  (echo-keystrokes                0.5         "Shows keystrokes right away.")
  (frame-title-format
    (list
      '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
      " (" user-login-name "@" system-name  ")"
      ) "Sets the frame title."
    )
  (indicate-buffer-boundaries     "left"      "Show buffer boundaries in left fringe.")
  (indicate-empty-lines           t           "Show empty lines in the left fringe.")
  (inhibit-startup-screen         t           "Do not show splash screen.")
  (initial-scratch-message        nil         "No scratch message.")
  (tool-bar-mode                  nil         "Do not show toolbar.")
  (transient-mark-mode            t           "Highlight sactive region.")
  (visible-bell                   t           "Uses visible bell."))

(use-package files
  :ensure nil
  :config
  (setq
    my-autosaves-dir               (no-littering-expand-var-file-name "auto-save/")
    auto-save-file-name-transforms `((".*" ,my-autosaves-dir t))
    auto-save-list-file-prefix     my-autosaves-dir)
  :custom
  (auto-save-default              t           "Auto-save of each file-visiting buffer.")
  (auto-save-interval             200         "Number of keystrokes before auto-save.")
  (auto-save-timeout              20          "Number of seconds idle time before auto-save.")
  (backup-by-copying              t           "Always use copying to create backup files.")
  (delete-old-versions            t           "Delete excess backup files silently.")
  (kept-new-versions              10          "Number of newest versions to keep when a new numbered backup is made. Includes the new backup.")
  (kept-old-versions              5           "Number of oldest versions to keep when a new numbered backup is made.")
  (make-backup-files              t           "Make numeric backup versions unconditionally.")
  (version-control                t           "Version numbers for backup files."))

(use-package font-core
  :ensure nil
  :config
  (global-font-lock-mode         1))          ; Toggle font Lock mode in all buffers

(use-package font-lock
  :ensure nil
  :custom
  (font-lock-maximum-decoration  t            "Uses maximum decoration."))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode           t))          ; Enable global hl-line mode

(use-package mouse
  :ensure nil
  :custom
  (mouse-buffer-menu-mode-mult   20          "Longer mouse buffer menu (major mode)."))

(use-package org-mode
  :ensure nil
  :bind (("C-c p" .
           (lambda ()
             (interactive)
             (find-file "~/Org/Perso/para.org"))))
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq my-perso-org-dir (expand-file-name "~/Org/Perso"))
  :custom
  (org-descriptive-links       t                 "Show hyperlinks decorated (not plain text).")
  (org-log-done                t                 "Insert the timestamp when a task is marked DONE.")
  (org-startup-indented        'indet            "Prefer the alternate stars and indent. scheme.")
  (org-agenda-files            '("~/Org/Perso/") "Make available all .org files for C-c C-w (org-refile)")
  (org-refile-targets          '(
				                          (nil :maxlevel . 4)
				                          (org-agenda-files :maxlevel . 4))
		"Specify targets for refile.")
  (org-refile-use-outline-path 'file             "Include the file name (without directory) into the path.")
  )

(use-package paren
  :ensure nil
  :config
  (show-paren-mode               t)          ; visualization of matching parensenable paren mode
  (setq
    show-paren-delay             nil         ; Highlights parenthesis without delay.
    show-paren-style             'mixed      ; Shows the matching paren if visible, the  expression otherwise.
    ))

(use-package recentf
  :ensure nil
  :defer 10
  :commands(recentf-add-file recentf-apply-filename-handlers recentf-mode )
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
  (recentf-max-menu-items        50          "How many recent files to keep in menu?")
  (recentf-max-saved-items       50          "How many recent files to save in file?")
  :config
  (recentf-mode                  t))         ; Enable recent files

(use-package saveplace
  :ensure nil
  :unless noninteractive
  :config
  (save-place-mode               t))         ; Automatically save place in each file.

(use-package scroll-bar
  :ensure nil
  :unless noninteractive
  :custom
  (scroll-bar-mode               "right"     "Use scroll bar on right"))

(use-package server
  :ensure nil
  :custom
  (server-auth-dir                           ; Directory for server auth files
    (expand-file-name "server-auth/" no-littering-var-directory)))

(use-package sgml-mode
  :ensure nil
  :commands sgml-mode
  :custom
  (sgml-quick-keys               'close      "Auto close elements upon '</'."))

(use-package simple
  :ensure nil
  :custom
  (column-number-mode            t           "Displays column number.")
  (size-indication-mode          t           "Show buffer size in mode line."))

(use-package vc-hooks
  :ensure nil
  :custom
  (vc-follow-symlinks            t         "Visits symlinks, don't follow them.")
  (vc-make-backup-files          t           "Backups version controlled files."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3rd party Emacs packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO Check whether desirable to use again these packages:
;; - counsel
;; - json-mode (Does emacs 27 include it now?)
;; - restclient (packageinstall error so far...)
;; - Ivi ?
;;   ivy-use-virtual-buffers t                    ;; Add recent files and bookmarks to ‘ivy-switch-buffer
;;   ivy-count-format "(%d/%d) "                  ;; Display the current candidate count for `ivy-read' to display both the index and the count.

(use-package all-the-icons)

(use-package auto-package-update
  :init
  (setq                                             ; Override "last update day" file in no-littering var dir
    apu--last-update-day-filename
    (expand-file-name
      ".auto-package-update-last-package-update-day"
      no-littering-var-directory))
  (setq auto-package-update-prompt-before-update t) ; Manual prompt before automatic updates"
  :config
  (auto-package-update-maybe))

(use-package color-theme-sanityinc-tomorrow
  :config
  (defun my-load-theme-dark ()  "Load the preferred dark theme."
	  (interactive)
	  (load-theme 'sanityinc-tomorrow-night t))
  (defun my-load-theme-light () "Load the preferred light theme."
	  (interactive)
	  (load-theme 'sanityinc-tomorrow-day t))
  (my-load-theme-light))        ; Load tomorrow-day theme by default

(use-package dashboard
  :disabled
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-items             '(
                                  (recents  . 10)
                                  (bookmarks . 5)
                                  (agenda . 20)
                                  (registers . 5))
		"Set the items to put at startup" )
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons    t)
  (dashboard-set-navigator     t)
  (dashboard-startup-banner    'logo))

(use-package doom-modeline
  :init
  (doom-modeline-mode        t)       ; Enable Doom mode line
  :custom
  (doom-modeline-minor-modes t        "Display the minor modes in the mode-line."))

(use-package editorconfig
  :config
  (editorconfig-mode t))               ; Enable EditorConfig

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

(use-package groovy-mode
  :mode ("\\.groovy\\'" . groovy-mode))

(use-package indium
  :bind
  ("C-x c" . indium-connect)
  :init
  (setq
    indium-chrome-data-dir
    (expand-file-name "indium-chrome-profile" no-littering-var-directory))
  (make-directory indium-chrome-data-dir t))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode))

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package markdown-mode
  :mode ( ("\\.md\\'"          . markdown-mode)
          ("\\.markdown\\'"    . markdown-mode)))

(use-package org-bullets
  :commands (org-bullets-mode)
  :init
  (add-hook 'org-mode-hook
	  (lambda ()
	    (org-bullets-mode t))))

(use-package restclient
  :disabled
  :mode ("\\.rest\\'" . restclient-mode))

(use-package treemacs
  :bind
  ("C-x t" . treemacs)
  :custom-face
  (treemacs-file-face      ((t (:height 1.0))))
  (treemacs-directory-face ((t (:height 1.0))))
  (treemacs-root-face      ((t (:height 1.0)))))

(use-package web-mode
  :mode
  ("\\.html\\'" . web-mode)
  ("\\.ejs\\'" . web-mode))

(use-package which-key
  :defer 5
  :commands which-key-mode
  :config
  (which-key-mode))

;;; init.el ends here
