;; Add to load path any sub dirs found in my-elisp-dir
(dolist (project (directory-files my-elisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Set up a bunch of emacs locations / paths
(setq

  ;; Where to store runtime directories (backups, autosave, tramp...)?
  my-runtime-data (expand-file-name "../.emacs.d.my-runtime-dirs/" user-emacs-directory)

  ;; Where do we put the ELPA packages?
  package-user-dir (expand-file-name "elpa/" my-runtime-data)

  ;; Where do we put the recentf file, and how many do we keep?
  recentf-save-file (expand-file-name "recentf" my-runtime-data)
  recentf-max-menu-items 50

  ;; Where do we backup?
  my-backup-dir (expand-file-name "backups/" my-runtime-data)

  ;; And how do we backup? (yes, it's a bit paranoid...)
  backup-directory-alist  `((".*" . ,my-backup-dir)) ; Backup all files in my-backup-dir
  backup-by-copying t    ; don't clobber symlinks@
  make-backup-files t    ; backup of a file the first time it is saved.
  delete-old-versions t  ; delete excess backup files silently
  version-control t      ; version numbers for backup files
  kept-old-versions 5    ; oldest versions to keep when a new numbered backup is made (default: 2)
  kept-new-versions 10   ; newest versions to keep when a new numbered backup is made (default: 2)
  vc-make-backup-files t ; make backups of files, even when they're in version control

  ;; Where do we auto save?
  my-autosaves-dir (expand-file-name "autosaves/" my-runtime-data)

  ;; And how do we auto save?
  auto-save-file-name-transforms `((".*" ,my-autosaves-dir t))
  auto-save-list-file-prefix my-autosaves-dir
  auto-save-default t    ; auto-save every buffer that visits a file
  auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
  auto-save-timeout 20   ; number of seconds idle time before auto-save (default: 30)

  ;; Where to store the server file?
  server-auth-dir (expand-file-name "server-auth/" my-runtime-data)

  ;; Where are stored the places?
  save-place-file (expand-file-name "places.el" my-runtime-data)

  ;; Where do we store the games scores ?
  gamegrid-user-score-file-directory (expand-file-name "games/" my-runtime-data)

  ;; Where do we auto save Tramp files?
  tramp-auto-save-directory (expand-file-name "tramp-autosaves/" my-runtime-data)

  ;; Keep emacs custom-settings in a separate file
  custom-file (expand-file-name "custom.el" my-elisp-dir)

  ;; Which lisp to use in slime ? Either "/usr/local/bin/sbcl" or  "/usr/local/bin/ccl64"
  inferior-lisp-program "/usr/local/bin/sbcl"
  )

;; Load custom settings now
(load custom-file)

(provide 'my-dirs)
