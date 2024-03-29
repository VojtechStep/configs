;;; md.rc.el --- Configuration for markdown files  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package markdown-mode
  :straight t
  :custom
  (markdown-command "marp")
  :custom-face
  (markdown-header-face-1 ((t (:height 1.3 :inherit markdown-header-face))))
  :hook
  (markdown-mode . (lambda ()
                     (require 'prose.rc)
                     (prose-mode))))

(provide 'markdown.rc)

;;; markdown.rc.el ends here
