;; DESCRIPTION:
;;   Enhancements to markdown-mode.
;;
;; AUTHOR:
;;   Geoffrey Grosenbach http://peepcode.com
;;
;; FEATURES:
;;
;;   * Adds Markdown headings to ido-menu
;;
;; USAGE:
;;   (require 'topfunky/tf-markdown)

(setq topfunky-markdown-imenu-generic-expression
      '(("Top-level Heading" "#\+ \\(\.\*\\)" 1)
        ))

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq imenu-generic-expression topfunky-markdown-imenu-generic-expression)))

(provide 'topfunky/markdown)
