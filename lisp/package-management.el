;; package management

(load "package")

(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar mnutt/packages '(autopair
                         better-defaults
                         coffee-mode
                         color-theme-sanityinc-tomorrow
                         company
                         company-flow
                         cyberpunk-theme
                         deft
                         feature-mode
                         flow-minor-mode
                         flycheck
                         flycheck-jest
                         gist
                         go-mode
                         graphviz-dot-mode
                         haml-mode
                         htmlize
                         js2-mode
                         js2-refactor
                         magit
                         markdown-mode
                         marmalade
                         maxframe
                         nodejs-repl
                         org
                         paredit
                         php-mode
                         prettier
                         restclient
                         rvm
                         rust-mode
                         smex
                         solarized-theme
                         tide
                         typescript-mode
                         web-beautify
                         web-mode
                         writegood-mode
                         xref-js2
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

(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'capnp-mode)

(provide 'package-management)
