;;; haskell.rc.el --- Configuration for Haskell files  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package haskell-mode
  :straight t
  :custom
  (haskell-hoogle-command "stack hoogle --")
  (haskell-mode-stylish-haskell-path "brittany")
  (haskell-process-type 'stack-ghci)
  :hook
  (haskell-mode . apheleia-mode)
  (haskell-mode . interactive-haskell-mode)
  :config
  (when (require 'apheleia nil t)
    (cl-pushnew '(brittany "brittany") apheleia-formatters)
    (cl-pushnew '(haskell-mode . brittany) apheleia-mode-alist)))

(use-package dante
  :straight t
  :after haskell-mode
  :commands 'dante-mode
  ;; :hook
  ;; (haskell-mode . dante-mode)
  :config
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint)))

;; (use-package lsp-haskell
;;   :defer t
;;   :hook (haskell-mode . lsp)
;;   :custom
;;   (lsp-haskell-process-path-hie "ghcide")
;;   (lsp-haskell-process-args-hie '())
;;   :config
;;   (cl-pushnew 'haskell-error-mode evil-emacs-state-modes))

(provide 'haskell.rc)

;;; haskell.rc.el ends here
