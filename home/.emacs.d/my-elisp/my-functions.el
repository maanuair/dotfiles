;; Define a couple of function to reformat / refontify the current buffer
(defun indent-buffer ()
  "Indent the currently visited buffer."
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
