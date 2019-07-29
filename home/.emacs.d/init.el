;;; init.el --- A regular emacs init file.

;; Copyright © 2016 Emmanuel Roubion

;; Author: Emmanuel Roubion
;; URL: https://github.com/maanuair/dotfiles

;; This file is NOT part of GNU Emacs.

;; Define my-elisp dir, and set up load path
(setq my-elisp-dir (expand-file-name "my-elisp" user-emacs-directory))
(add-to-list 'load-path my-elisp-dir)

;; OK, so set up directories the way I want them
(require 'my-dirs)

;; Then perform some packages management
(require 'package)
(add-to-list 'package-archives
  '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'my-packages)

;; Load some common functions
(require 'my-functions)

;; A few UI appearanes and behaviors
(require 'my-ui)

;; Finally load a few settings abour org-mode
(require 'my-org)
