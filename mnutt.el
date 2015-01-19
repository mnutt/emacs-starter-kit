;; DESCRIPTION: topfunky settings

(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

(setq ns-auto-hide-menu-bar t)

;; for homebrew:
(setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))

;; Save backups in one place
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
  (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq create-lockfiles nil)

(setq default-tab-width 2)
(setq tab-width 2)
(setq javascript-indent-mode 2)

;;; Shell mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)


;; Yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Clojure
;;(eval-after-load 'clojure-mode '(clojure-slime-config))

;; Plain Text
;;; Stefan Monnier <foo at acm.org>. It is the opposite of
;;; fill-paragraph. Takes a multi-line paragraph and makes
;;; it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun refresh-file ()
  (interactive)
  (revert-buffer t t t))
(global-set-key [f5] 'refresh-file)

;; Snippets
;; (add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet.el"))
;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory (concat dotfiles-dir "/vendor/yasnippet.el/snippets"))

;; Commands
(require 'unbound)

;; Prevent annoying bug "The mark is not set now so there is no region".
;; Occurs with interactive "r" defuns.
(set-mark (point))
(deactivate-mark)

;; Minor Modes
(add-to-list 'load-path (concat dotfiles-dir "/vendor/textmate.el"))
(require 'textmate)
(textmate-mode)
(require 'whitespace)
(require 'ansi-color)

;; Major Modes

;; enables outlining for ruby
(add-hook 'ruby-mode-hook
              '(lambda ()
                 (outline-minor-mode)
                 (setq outline-regexp " *\\(def \\|class\\|module\\)")))

;; Turn on coloring for shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Remove scrollbars and make hippie expand
;; work nicely with yasnippet
(scroll-bar-mode -1)
(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        ;;        try-expand-dabbrev-from-kill
        ;;         try-complete-file-name
        ;;         try-complete-file-name-partially
        ;;         try-complete-lisp-symbol
        ;;         try-complete-lisp-symbol-partially
        ;;         try-expand-line
        ;;         try-expand-line-all-buffers
        ;;         try-expand-list
        ;;         try-expand-list-all-buffers
        ;;        try-expand-whole-kill
        ))

;; (defun indent-or-complete ()
;;   (interactive)
;;   (if (and (looking-at "$") (not (looking-back "^\\s-*")))
;;       (hippie-expand nil)
;;     (indent-for-tab-command)))
;; (add-hook 'find-file-hooks (function (lambda ()
;;                                        (local-set-key (kbd "TAB") 'indent-or-complete))))

;; dabbrev-case-fold-search for case-sensitive search

;; Rinari
(add-to-list 'load-path (concat dotfiles-dir "/vendor/jump.el"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rinari"))
(require 'rinari)
(define-key rinari-minor-mode-map [(control meta shift down)] 'rinari-find-rspec)
(define-key rinari-minor-mode-map [(control meta shift left)] 'rinari-find-controller)
(define-key rinari-minor-mode-map [(control meta shift up)] 'rinari-find-model)
(define-key rinari-minor-mode-map [(control meta shift right)] 'rinari-find-view)

;; rspec mode
(setq rspec-use-rake-flag 0)

;; rhtml mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rhtml"))
(require 'rhtml-mode)

;; applescript mode
(autoload 'applescript-mode "applescript-mode" "major mode for editing AppleScript source." t)
(setq auto-mode-alist
      (cons '("\\.applescript$" . applescript-mode) auto-mode-alist))

;; org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; coffee mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/coffee-mode"))
(require 'coffee-mode)

(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; textile mode
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

;; haml mode
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(define-key haml-mode-map [(control meta down)] 'haml-forward-sexp)
(define-key haml-mode-map [(control meta up)] 'haml-backward-sexp)
(define-key haml-mode-map [(control meta left)] 'haml-up-list)
(define-key haml-mode-map [(control meta right)] 'haml-down-list)

;; sass mode
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(add-to-list 'auto-mode-alist '("\\.sake\\'" . ruby-mode))

;; fix css mode
(require 'css-mode)
(setq cssm-indent-level 2)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-mirror-mode t)
(setq css-indent-offset 2)

;; XCODE
(require 'objc-c-mode)

;; (setq c-default-style "bsd"
;;       c-basic-offset 2)

(require 'cc-menus)

(require 'xcode)
(define-key objc-mode-map [(meta r)] 'xcode-compile)
(define-key objc-mode-map [(meta K)] 'xcode-clean)
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  [(meta O)] 'ff-find-other-file)))
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>")  'hs-hide-block)
            (local-set-key (kbd "C-c <up>")    'hs-hide-all)
            (local-set-key (kbd "C-c <down>")  'hs-show-all)
            (hs-minor-mode t)))         ; Hide and show blocks
(add-to-list 'auto-mode-alist '("\\.h\\'" . objc-mode))
(require 'objj-mode)

;; Rsense for ruby completion
;; (setq rsense-home "/usr/local/Cellar/rsense/0.3")
;; (require 'rsense)

;; Show line numbers
;; (require 'line-num)
;; (global-linum-mode 1)

;; Font
(set-face-font 'default "-apple-inconsolata-medium-r-normal--16-0-72-72-m-0-iso10646-1")
(set-face-attribute 'default nil :family "Menlo" :height 170 :weight 'normal)

;; Color Themes
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(require 'color-theme)
(color-theme-initialize)
(set-face-background 'region "grey40")

;; Functions

;; Full screen toggle
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))
(global-set-key (kbd "M-n") 'toggle-fullscreen)


;; Keyboard

;; Automatically indent on newline
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Split Windows
(global-set-key [f6] 'split-window-horizontally)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'delete-window)

(global-set-key [(meta up)] 'beginning-of-buffer)
(global-set-key [(meta down)] 'end-of-buffer)

(global-set-key [(meta shift right)] 'ido-switch-buffer)
(global-set-key [(meta shift up)] 'recentf-ido-find-file)
(global-set-key [(meta shift down)] 'ido-find-file)
(global-set-key [(meta shift left)] 'magit-status)

(global-set-key [(meta H)] 'delete-other-windows)

(global-set-key [(meta D)] 'backward-kill-word) ;; (meta d) is opposite

(global-set-key [(meta N)] 'cleanup-buffer)

(global-set-key [(control \])] 'indent-rigidly)

;; Other

(prefer-coding-system 'utf-8)

(server-start)

;; Activate theme
(color-theme-vivid-chalk)

(require 'maxframe)
;; (add-hook 'window-setup-hook 'maximize-frame t)

(provide 'mnutt)
