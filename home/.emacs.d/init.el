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

;; Finally set up ui and some behaviors
(require 'my-ui)

;; TODO:
;; Check out ido-mode / anything (aka spotlight for emacs) / gnus

;; Some emacs links to learn more:
;;
;;    Emacs is sexy
;;    http://emacs.sexy/
;;
;;    Emacs Redux | Return to the Essence of Text Editing
;;    http://emacsredux.com/
;;
;;    Making Emacs Work For Me
;;    http://zeekat.nl/articles/making-emacs-work-for-me.html
;;
;;    CLOJURE for the BRAVE and TRUE
;;    http://www.braveclojure.com/basic-emacs/)
;;
;;    Mastering Emacs
;;    http://www.masteringemacs.org/
;;    http://www.masteringemacs.org/article/introduction-magit-emacs-mode-git
;;
;;    Emacs Rocks
;;    http://emacsrocks.com
;;
;;    What the .emacs.d ?
;;    http://whattheemacsd.com/
;;
;;    It is not hard to edit Lisp code
;;    http://yoo2080.wordpress.com/2014/07/20/it-is-not-hard-to-edit-lisp-code/
;;
;;    Start using emacs:
;;    http://www.braveclojure.com/basic-emacs/
