;;; evil.rc.el --- Configuration for evil-mode

;;; Commentary:
;; 

;;; Code:

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
  :defines evil-motion-state-modes
  :straight t
  :demand
  :custom
  (evil-want-keybinding nil)
  (evil-lookup-func #'vs/help-dwim)
  (evil-symbol-word-search t)
  (evil-shift-width tab-width)
  (evil-insert-state-cursor '(bar . 5))
  :config
  (dolist (m '(dired-mode
               package-menu-mode
               debugger-mode
               use-package-statistics-mode
               flycheck-error-list-mode))
    (cl-pushnew m evil-motion-state-modes))
  (dolist (m '(dired-mode-map
               compilation-mode-map))
    (evil-add-hjkl-bindings m))
  (dolist (f '(evil-line-move
               evil-backward-char
               evil-forward-char))
    (advice-add f :around #'vs/evil-move-noerror))
  (evil-mode))

(use-package evil-magit
  :straight t
  :hook
  (magit-mode . (lambda ()
                  (require 'evil-magit))))

;; Cursor change in terminal
(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :straight t
    :demand
    :config
    (setq evil-motion-state-cursor 'box)
    (setq evil-visual-state-cursor 'box)
    (setq evil-normal-state-cursor 'box)
    (setq evil-insert-state-cursor 'bar)
    (setq evil-emacs-state-cursor 'bar)
    (etcc-on)))

;; Evil in org mode
(use-package evil-org
  :straight t
  :after (evil org)
  :config
  (evil-org-set-key-theme))

;; Sniping
(use-package evil-snipe
  :straight t
  :after evil
  :hook
  (magit-mode . turn-off-evil-snipe-override-mode)
  :custom
  (evil-snipe-scope 'visible)
  :config
  (evil-snipe-override-mode t))

;; Surround
(use-package evil-surround
  :straight t
  :demand
  :after evil
  :config
  (global-evil-surround-mode 1))
  
(use-package evil-goggles
  :straight t
  :demand
  :after evil
  :config
  (evil-goggles-mode t))

(provide 'evil.rc)

;;; evil.rc.el ends here
