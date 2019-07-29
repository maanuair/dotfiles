;; Everything related to my Org mode usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A function to open my default org file
(defun load-my-org ()
  "Loads my org file, stored by default in \"~/org/index.org.\""
  (interactive)
  (find-file "~/org/index.org" nil)
  )

;; Some global custom shortcuts
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c o") 'load-my-org)

;; Some Org mode settings
(setq
  org-descriptive-links nil    ;; Show hyperlinks in plain text
  org-log-done t               ;; Insert the timestamp when a task is marked DONE
  org-startup-indented 'indet  ;; Prefer the alternate stars and indent. scheme
  )

(provide 'my-org)
