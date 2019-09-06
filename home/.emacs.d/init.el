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

;; Define my-elisp dir, and set up load path
(setq my-elisp-dir (expand-file-name "my-elisp" user-emacs-directory))
(add-to-list 'load-path my-elisp-dir)

;; OK, so set up directories the way I want them
(require 'my-dirs)

;; Initialize my packages...
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'my-packages)

;; Load some custom functions
(require 'my-functions)

;; A few UI appearanes and behaviors
(require 'my-ui)

;; Finally load a few settings abour org-mode
(require 'my-org)
