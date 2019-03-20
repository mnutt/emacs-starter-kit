;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

(setq user-full-name "Michael Nutt")
(setq user-mail-address "michael@nutt.im")

(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq package-user-dir (concat dotfiles-dir "../elpa"))
(setq lisp-dir (concat dotfiles-dir "lisp/"))
(add-to-list 'load-path lisp-dir)
(setq custom-file (concat lisp-dir "custom.el"))

(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:/Users/michael/go/bin:/Users/michael/.rvm/bin" (getenv "PATH")))
(require 'cl)

(require 'package-management)
(require 'windowing)
(require 'keybindings)
(require 'autosave)
(require 'indentation)
(require 'mode-setup)

;; load theme
(load-theme 'sanityinc-tomorrow-night t)

(require 'better-defaults)

;; configure smex for better M-x
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; lockfiles and ember don't get along
(setq create-lockfiles nil)

;; ido
(setq ido-use-virtual-buffers t)

;; json uses 2 spaces
(setq js-indent-level 2)

;; auto close parens
(require 'autopair)

(provide 'init)
