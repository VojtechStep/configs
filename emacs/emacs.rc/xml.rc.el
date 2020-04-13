;;; xml.rc.el --- Configuration for XML files

;;; Commentary:
;; 

;;; Code:

(use-package nxml-mode
  :straight (nxml-mode :type built-in)
  :custom
  (nxml-slash-auto-complete-flag t))

(provide 'xml.rc)

;;; xml.rc.el ends here
