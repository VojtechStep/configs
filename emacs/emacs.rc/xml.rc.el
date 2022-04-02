;;; xml.rc.el --- Configuration for XML files  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package nxml-mode
  :straight (nxml-mode :type built-in)
  :mode "\\.\\(f\\|c\\)sproj\\'"
  :custom
  (nxml-slash-auto-complete-flag t))

(provide 'xml.rc)

;;; xml.rc.el ends here
