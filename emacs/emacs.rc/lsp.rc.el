;;; lsp.rc.el --- Configuration for LSP clients

;;; Commentary:
;; 

;;; Code:

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :custom
;;   (lsp-enable-snippet nil) ; Might revisit
;;   (lsp-enable-semantic-highlighting t)
;;   (lsp-eldoc-hook nil)			; Cannot fit on single line, gets annoying
;;   (lsp-enable-symbol-highlighting nil)
;;   (lsp-lens-auto-enable t)
;;   (lsp-ui-sideline-show-diagnostic nil)
;;   ;; (lsp-prefer-capf t)
;;   ;; :custom-face
;;   ;; (lsp-face-highlight-textual ((t (:background "#373b61"))))
;;   ; (lsp-face-highlight-read ((t (:inherit lsp-face-highlight-textual))))
;;   :config
;;   (require 'lsp-clients))

;; (use-package nox
;;   :load-path "~/Code/VojtechStep/Projects/nox/"
;;   :commands (nox nox-ensure)
;;   :hook
;;   ((c-mode
;;     c++-mode
;;     java-mode
;;     rust-mode) .
;;     nox-ensure
;;     )
;;   :custom
;;   (nox-doc-tooltip-font-size 8)
;;   (nox-autoshutdown t)
;;   :config
;;   (cl-pushnew '(java-mode . ("~/Code/VojtechStep/Projects/java-language-server/dist/lang_server_linux.sh")) nox-server-programs))

;; (use-package eglot
;;   :hook
;;   ((c-mode
;;     c++-mode
;;     java-mode
;;     rust-mode) .
;;     eglot))

;; (use-package lsp-ui
  ;; :ensure t
  ;; :hook
  ;; (lsp-mode . lsp-ui-mode)
  ;; :custom
  ;; (lsp-prefer-flymake nil)
  ;; (lsp-ui-doc-enable nil)
  ;; (lsp-ui-doc-position 'at-point))

(use-package flycheck
  :straight t
  :custom
  (flycheck-display-errors-delay 0.1)
  (flycheck-emacs-lisp-load-path 'inherit)
  :hook
  (prog-mode . flycheck-mode))

(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-idle-delay 0.25)
  (eldoc-echo-area-use-mutliline-p nil)
  :hook
  (prog-mode . eldoc-mode)
  :config
  (defun vs/--ignore-eldoc-when-flycheck (&rest _)
    (not (and (fboundp 'flycheck-overlay-errors-at)
              (flycheck-overlay-errors-at (point)))))
  (advice-add #'eldoc-display-message-no-interference-p
              :after-while
              #'vs/--ignore-eldoc-when-flycheck)
  (global-eldoc-mode))

;; (use-package dap-mode
;;   :defer t
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t))

(provide 'lsp.rc)

;;; lsp.rc.el ends here
