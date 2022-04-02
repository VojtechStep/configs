;;; evil.rc.el --- Configuration for evil-mode  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'vs-utils.rc)
(eval-when-compile
  (require 'use-package))


(defun vs/evil-move-noerror (orig-fun &rest args)
  "Wrap ORIG-FUN passing ARGS and catch inner errors.
Used as an :around advice for movement in 'evil-mode',
so that it suppresses those annoying Beggining/End of buffer/line messages.

This catches the error inside. That is because when the movement functions
get a noerror argument, they call themselves without it and wrap the call
in a 'condition-case. That in turn calls this advice and repeat infinitely."
  (condition-case nil
      (apply orig-fun args)
    (error nil)))

(use-package evil
  :straight t
  :demand
  :custom
  (evil-ex-substitute-global t)
  (evil-lookup-func #'vs/help-dwim)
  (evil-respect-visual-line-mode t)
  (evil-shift-width tab-width)
  (evil-split-window-below t)
  (evil-symbol-word-search t)
  (evil-vsplit-window-right t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-keybinding nil)
  (evil-want-fine-undo t)
  (evil-want-visual-char-semi-exclusive t)
  (evil-echo-state nil)
  ;; (evil-search-module 'evil-search)
  (evil-undo-system 'undo-redo)
  :config
  (dolist (f '(evil-line-move
               evil-backward-char
               evil-forward-char))
    (advice-add f :around #'vs/evil-move-noerror))
  (evil-set-initial-state 'bluetooth-mode 'insert)
  (evil-mode))

(use-package evil-collection
  :straight t
  :after evil)

;; Sniping
(use-package evil-snipe
  :straight t
  :after evil
  :hook
  (magit-mode . turn-off-evil-snipe-override-mode)
  :custom
  (evil-snipe-scope 'visible)
  (evil-snipe-repeat-keys nil)
  :config
  (evil-snipe-mode))

;; Surround
(use-package evil-surround
  :straight t
  :hook
  (vs/first-input . (lambda ()
                      (require 'evil-surround)))
  (org-mode
   . (lambda ()
       (cl-pushnew '(?$ . ("$" . "$"))
                   evil-surround-pairs-alist)))
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-goggles
  :straight t
  :hook
  (vs/first-input . (lambda ()
                      (require 'evil-goggles)))
  :after evil
  :config
  (evil-goggles-mode))

(use-package evil-commentary
  :straight t
  :hook
  (vs/first-input . (lambda ()
                      (require 'evil-commentary)))
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-numbers
  :straight t
  :after evil)

(provide 'evil.rc)

;;; evil.rc.el ends here
