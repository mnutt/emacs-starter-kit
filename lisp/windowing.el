;; fullscreen
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))
(global-set-key (kbd "M-n") 'toggle-fullscreen)

;; fix annoying macos visual bell
(setq ring-bell-function (lambda () (message "*beep*")))

;; access via emacsclient
(server-start)

;; column numbers
(setq column-number-mode t)

;; nicer windowing
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (set-face-attribute 'default nil
                      :family "Inconsolata"
                      :height 140
                      :weight 'normal
                      :width 'normal)

  (when (functionp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      'unicode
                      (font-spec :family "DejaVu Sans Mono"
                                 :width 'normal
                                 :size 12.4
                                 :weight 'normal))))


;; Skip the splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)


(setq echo-keystrokes 0.1
      use-dialog-box nil)
(show-paren-mode t)

;; font
(set-face-attribute 'default nil :family "Menlo" :height 170 :weight 'normal)

;; Prevent annoying bug "The mark is not set now so there is no region".
;; Occurs with interactive "r" defuns.
(set-mark (point))
(deactivate-mark)

(provide 'windowing)
