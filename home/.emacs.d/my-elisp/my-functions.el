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

(defun indent-buffer ()
  "Indent the whole currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun reformat ()
  "Indent whole buffer, removes trailing spaces, and refontify it. Note that the EditorConfig package has its own formating settings."
  (interactive)
  (indent-buffer)
  (delete-trailing-whitespace)       ;; EditorConfig may override
  (untabify (point-min) (point-max)) ;; EditorConfig may override
  (font-lock-fontify-buffer)
  )

;; Are we a Mac or a Win?
(defun is-mac ()
  (equal system-type 'darwin))
(defun is-win ()
  (equal system-type 'windows-nt))

(provide 'my-functions)
