;;; lsp.rc.el --- Configuration for LSP clients  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-enable-snippet nil) ; Might revisit
  (lsp-enable-semantic-highlighting t)
  (lsp-log-io t)
  ;; (lsp-eldoc-hook nil)			; Cannot fit on single line, gets annoying
  (lsp-enable-symbol-highlighting nil)
  ;; (lsp-lens-auto-enable t)
  (lsp-completion-provider :capf)
  (lsp-keep-workspace-alive nil)
  ;; :custom-face
  ;; (lsp-face-highlight-textual ((t (:background "#373b61"))))
  ; (lsp-face-highlight-read ((t (:inherit lsp-face-highlight-textual))))
  )

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
  (flycheck-emacs-lisp-initialize-packages nil)
  ;; (flycheck-emacs-lisp-executable "emacs") ; Workaround because https://debbugs.gnu.org/cgi/bugreport.cgi?bug=37847
  ;; (flycheck-display-errors-function #'vs/flycheck-display-error-messages)
  :hook
  (prog-mode . flycheck-mode)
  :config
  (defun vs/flycheck-display-error-messages (errors)
    (when (and errors (flycheck-may-use-echo-area-p))
      (require 'seq)
      (let ((messages (seq-map #'flycheck-error-format-message-and-id
                               errors))
            (errors-buffer (get-buffer
                            flycheck-error-message-buffer)))
        (if errors-buffer
            (with-current-buffer errors-buffer
              (erase-buffer)
              (insert (string-join messages "\n\n"))
              (display-buffer errors-buffer 'not-this-window))
          (flycheck-display-error-messages errors)))))
  (require 'evil-collection)
  (evil-collection-flycheck-setup))

(use-package eldoc
  :straight (eldoc :type built-in)
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

(use-package apheleia
  :straight (apheleia :host github
                      :repo "raxod502/apheleia")
  :hook
  (apheleia-mode . (lambda ()
                     (remove-hook 'after-save-hook #'delete-trailing-whitespace))))

(use-package compile
  :custom
  (compilation-scroll-output t)
  (compilation-always-kill t)
  :hook
  (compilation-filter . vs/colorize-compilation)
  :config
  (setq compilation-ask-about-save nil) ; Putting it in `:custom' force loads the package
  (defun vs/colorize-compilation ()
    (when (require 'ansi-color nil t)
      (setq-local show-trailing-whitespace nil)
      (let ((readonly buffer-read-only))
        (read-only-mode -1)
        (ansi-color-apply-on-region
         compilation-filter-start (point))
        (read-only-mode (or readonly -1)))))
  (require 'evil-collection)
  (evil-collection-compile-setup)
  (evil-collection-inhibit-insert-state 'compilation-mode-map))

;; (use-package dap-mode
;;   :defer t
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t))

(use-package flycheck-package
  :straight t
  :demand t
  :after flycheck
  :config
  (flycheck-package-setup))


(provide 'lsp.rc)

;;; lsp.rc.el ends here
