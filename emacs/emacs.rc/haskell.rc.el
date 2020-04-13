;;; haskell.rc.el --- Configuration for Haskell files

;;; Commentary:
;; 

;;; Code:

(use-package haskell-mode
  :straight t
  :custom
  (haskell-hoogle-command "stack hoogle --")
  (haskell-mode-stylish-haskell-path "brittany")
  (haskell-process-type 'stack-ghci))

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
