;;; js.rc.el --- Configuration for JavaScript files  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(use-package js-mode
  :hook
  (js-mode . lsp))

(provide 'js.rc)

;;; js.rc.el ends here
