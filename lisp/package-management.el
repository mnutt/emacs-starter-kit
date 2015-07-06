;; package management

(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar mnutt/packages '(ac-slime
                         ac-js2
                         auto-complete
                         autopair
                         better-defaults
                         coffee-mode
                         color-theme-sanityinc-tomorrow
                         cyberpunk-theme
                         deft
                         feature-mode
                         flycheck
                         gist
                         go-mode
                         graphviz-dot-mode
                         haml-mode
                         htmlize
                         js2-mode
                         magit
                         markdown-mode
                         marmalade
                         maxframe
                         nodejs-repl
                         org
                         paredit
                         php-mode
                         restclient
                         rvm
                         smex
                         solarized-theme
                         web-beautify
                         web-mode
                         writegood-mode
                         yaml-mode)
  "Default packages")

(defun mnutt/packages-installed-p ()
  (loop for pkg in mnutt/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

;; Install any new packages
(unless (mnutt/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg mnutt/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'package-management)
