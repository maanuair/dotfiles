;; Automatic install of packages
(defvar
  my-packages
  '(
    editorconfig
    emmet-mode
    groovy-mode
    js2-mode
    json-mode
    lua-mode
    markdown-mode
    magit
    mocha
    restclient
    solarized-theme
    web-mode
    )
  "The list of packages that should be installed, and verified as such at startup")

(defvar my-missing-packages '()
  "List that is populated at each startup, that will contain the list of packages that need to be installed.")

;; Populate the list of missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (add-to-list 'my-missing-packages p)))

;; Check internet connection
(setq my-onlinep nil)
(unless
    (condition-case nil
        (delete-process
         (make-network-process
          :name "my-check-internet"
          :host "elpa.gnu.org"
          :service 80))
      (error t))
  (setq my-onlinep t))

;; Automatically install the missing packages
(when (and my-missing-packages my-onlinep)
  (message "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (dolist (p my-missing-packages)
    (message "Installing `%s' .." p)
    (package-install p))
  (setq my-missing-packages '()))

(provide 'my-packages)
