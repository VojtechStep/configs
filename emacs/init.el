;;; init.el --- Entrypoint to my Emacs configuration

;;; Commentary:

;;; Code:

(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs ready in %ss with %d garbage collections taking up %ss"
              (float-time (time-subtract after-init-time before-init-time))
              gcs-done gc-elapsed)))

(require 'straight.rc)

;; Needs revisiting
(require 'editor.rc)
(require 'evil.rc)
(require 'auth.rc)
;; Seems a little bare
(require 'magit.rc)

;; Try making it usable and then use it
(require 'mail.rc)

;; Read more on company
(require 'company.rc)
(require 'nav.rc)

;; Make CMake-ccls more robust
;; Either ignore existing debug folders or generate them iteratively
(require 'c_cpp.rc)
(require 'csv.rc)
;; Proper REPL setup
(require 'haskell.rc)
;; Revisit
(require 'java.rc)
;; Read on using Emacs for JS first
;; (require 'js.rc)
;; Decide on a client, or write my own
(require 'lsp.rc)
;; Look into live preview
(require 'markdown.rc)
;; Do way more
(require 'org.rc)
;; Revisit later
(require 'pdf.rc)
;; Revisit later
(require 'rust.rc)
(require 'shell.rc)
;; Read on what you can do
(require 'sx.rc)
;; Setup properly
(require 'xml.rc)

(require 'general.rc)


(provide 'init)
;;; init.el ends here
