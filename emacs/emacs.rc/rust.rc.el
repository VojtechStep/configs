;;; rust.rc.el --- Configuration for Rust files  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package rustic
  :straight t
  :custom
  (rustic-lsp-setup-p nil))

(provide 'rust.rc)

;;; rust.rc.el ends here
