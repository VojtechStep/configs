;;; init.el --- Entrypoint to my Emacs configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Configure profiler
(defvar esup-depth 1)

;; Open per-project scratch files
(require 'vs-utils.rc)
(setq initial-buffer-choice #'vs/project-scratch-buffer)

(if (not (featurep 'early-init))
    (load "~/.config/emacs/early-init.el"))

(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs ready in %ss with %d garbage collections taking up %ss"
              (float-time (time-subtract after-init-time before-init-time))
              gcs-done gc-elapsed)
    (when-let ((scratch (get-buffer "*scratch*"))) ; Kill the original scratch file
      (kill-buffer scratch))))

(setq confirm-kill-processes nil
      global-auto-revert-mode t)

(setq-default
      resize-mini-windows t
      mouse-highlight 1)

;; For some reason this can't be in early-init
;; (setq auto-save-list-file-prefix nil)
(require 'straight.rc) ; done
(require 'env.rc) ; done
(require 'fonts.rc) ; done

(require 'modeline.rc) ; done

(require 'language.rc) ; done

(require 'evil.rc) ; done? evil-snipe
;; Needs revisiting
(require 'editor.rc) ; done
(require 'window-management.rc) ; toy with it one I hit issues
(require 'auth.rc) ; add when relevant
;; Seems a little bare
;; Fix unknown function
(require 'magit.rc) ; done? do I want forge?

;; Try making it usable and then use it
(require 'mail.rc)

(require 'eww.rc) ; done

;; Read more on company
(require 'company.rc)
(require 'nav.rc) ; done except for dired (and profiler)

;; Make CMake-ccls more robust
;; Either ignore existing debug folders or generate them iteratively
(require 'c_cpp.rc)
(require 'crystal.rc)
(require 'csharp.rc)
(require 'csv.rc)
(require 'docker.rc)
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
(require 'nix.rc)
(require 'org.rc)
;; Revisit later
(require 'pdf.rc)
(require 'python.rc)
;; Revisit later
(require 'rust.rc)
(require 'shell.rc)
;; Read on what you can do
(require 'sx.rc)
;; Setup properly
(require 'xml.rc)
(require 'yaml.rc)

(require 'zig.rc)

(require 'general.rc)

(require 'irc.rc)
(require 'elfeed.rc) ; done
(require 'transmission.rc)
(require 'ytdious.rc)

(use-package bluetooth
  :straight t)

(with-current-buffer "*Messages*"
  (setq-local vs/immortal t))

(provide 'init)
;;; init.el ends here
