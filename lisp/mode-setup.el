(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

(rvm-use-default)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2))

(add-hook 'coffee-mode-hook 'coffee-custom)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))

;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
;;(add-hook 'js2-mode-hook #'js2-refactor-mode)

(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(require 'company)
(require 'company-web-html)
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-to-list 'company-backends 'company-flow)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
(add-hook 'web-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

(defun my-web-mode-hook ()
  "Hook for `web-mode'."
    (set (make-local-variable 'company-backends)
         '(company-tern company-web-html company-yasnippet company-files)))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Enable JavaScript completion between <script>...</script> etc.
(advice-add 'company-tern :before
            #'(lambda (&rest _)
                (if (equal major-mode 'web-mode)
                    (let ((web-mode-cur-language
                          (web-mode-language-at-pos)))
                      (if (or (string= web-mode-cur-language "javascript")
                              (string= web-mode-cur-language "jsx"))
                          (unless tern-mode (tern-mode))
                        (if tern-mode (tern-mode -1)))))))

;; don't drop port files everywhere
(setq tern-command (append tern-command '("--no-port-file")))

;; Disable completion keybindings, as we use xref-js2 instead
;;(define-key tern-mode-keymap (kbd "M-.") nil)
;;(define-key tern-mode-keymap (kbd "M-,") nil)

(eval-after-load 'web-mode
  '(add-hook 'web-mode-hook #'add-node-modules-path))

;; enable prettier globally
(require 'compile)
(add-hook 'after-init-hook #'global-prettier-mode)

;; fix css mode
(setq cssm-indent-level 2)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-mirror-mode t)
(setq css-indent-offset 2)

(add-hook 'css-mode-hook (lambda ()
                           (company-mode)))

;; sass mode
(add-to-list 'auto-mode-alist '("\\.sass$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; Turn on coloring for shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically
      '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)

;; disable jshint/json since we prefer eslint checking
(setq-default flycheck-disabled-checkers '(javascript-jshint json-jsonlist js-lint))

(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

(with-eval-after-load 'web-mode
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(defun my/configure-web-mode-flycheck-checkers ()
  ;; in order to have flycheck enabled in web-mode, add an entry to this
  ;; cond that matches the web-mode engine/content-type/etc and returns the
  ;; appropriate checker.
  (-when-let (checker (cond
                       ((string= web-mode-content-type "jsx")
                        'javascript-eslint)))
    (flycheck-mode)
    (flycheck-select-checker checker)))

(add-hook 'web-mode-hook #'my/configure-web-mode-flycheck-checkers)


;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[jt]sx?\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-style-padding 2)
(setq web-mode-script-padding 2)

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.[jt]sx?\\'" . prettier-mode))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.[jt]sx?\\'" . js2-minor-mode))))


(setq js2-basic-offset 2)
(setq js2-bounce-indent-p nil)

;; typescript-mode
(setq typescript-indent-level 2)

;; rust-mode
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq rust-format-on-save t)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; to find node.js
(setq exec-path (append exec-path '("/Users/mnutt-mbp/.volta/bin")))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))

(provide 'mode-setup)
