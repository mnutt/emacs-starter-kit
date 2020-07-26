;; Save backups in one place
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!

(cond
  ((string-equal system-type "darwin")
    (defvar autosave-dir
      (concat "/Users/" (user-login-name) "/tmp/emacs_autosaves/" (user-login-name) "/")))
  ((string-equal system-type "gnu/linux")
       (defvar autosave-dir
      (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;; no backup files
(setq make-backup-files nil)

(provide 'autosave)
