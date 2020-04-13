;;; java.rc.el --- Configuration for Java files

;;; Commentary:
;; 

;;; Code:

;; (use-package lsp-java
;;   :defer t
;;   :hook
;;   (java-mode . (lambda ()
;;                  (require 'lsp-java)
;;                  (lsp)))
;;   :custom
;;   (lsp-java-save-actions-organize-imports t)
;;   (lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"))

;; (use-package dap-java
;;   :after lsp-java)

(use-package java-ide
  :load-path "~/Code/VojtechStep/Projects/emacs-java-ide"
  :hook
  (java-mode . (lambda ()
                 (require 'java-ide))))

(provide 'java.rc)

;;; java.rc.el ends here
