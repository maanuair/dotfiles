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

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Portion of heap used for allocation.  Defaults to 0.1.
(setq gc-cons-percentage 0.6)

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs ready in %s with %d garbage collections."
      (format "%.2f seconds"
        (float-time
          (time-subtract after-init-time before-init-time)))
      gcs-done)))

;; Define my-elisp dir, and set up load path
(setq my-elisp-dir (expand-file-name "my-elisp" user-emacs-directory))
(add-to-list 'load-path my-elisp-dir)

;; OK, so set up directories the way I want them
(require 'my-dirs)

;; Initialize my packages...
(require 'package)
(package-initialize)
(require 'my-packages)

;; Load some custom functions
(require 'my-functions)

;; A few UI appearanes and behaviors
(require 'my-ui)

;; Finally load a few settings about org-mode
(require 'my-org)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
