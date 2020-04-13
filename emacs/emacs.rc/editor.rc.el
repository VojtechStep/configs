;;; editor.rc.el --- Editor config  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

;; Builtins

;; Consider CamelCase and snake_case as different words
(use-package subword
  :demand
  :config
  (global-subword-mode))

;; Highlight matching paranthesis
(use-package paren
  :demand
  :custom
  (show-paren-delay 0)
  :config
  ;; Apparenly, just using set-face-attribute doesn't work, so I just
  ;; used a workaround as found on the Emacs Wiki
  (let ((face 'show-paren-match)
        (spec `((t .
                   (:foreground ,(face-attribute 'cursor :background)
                    :background ,nil
                    :box (:line-width -1)
                    :slant italic)))))
    (put face 'customized-face spec)
    (face-spec-set face spec))
  (show-paren-mode))

(use-package elec-pair
  :demand
  :config
  (electric-pair-mode))

(use-package autoinsert
  :demand
  :config
  (auto-insert-mode))

;; External

(use-package undo-tree
  :straight t
  :demand
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo"))))

(use-package telephone-line
  :straight t
  :demand
  :custom
  (telephone-line-height 24)
  (telephone-line-primary-left-separator 'telephone-line-flat)
  (telephone-line-primary-right-separator 'telephone-line-flat)
  (telephone-line-secondary-left-separator 'telephone-line-flat)
  (telephone-line-secondary-right-separator 'telephone-line-flat)
  :config
  (telephone-line-mode))

(use-package which-key
  :straight t
  :demand
  :config
  (which-key-mode))

(use-package activity-watch-mode
  :straight t
  :demand
  :config
  (global-activity-watch-mode))

(defvar vs/--loaded-theme)
(use-package color-theme-sanityinc-tomorrow
  :straight t
  :if (not vs/--loaded-theme)
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package hl-todo
  :straight t
  :demand
  :config
  (global-hl-todo-mode))

(use-package helpful
  :disabled
  :straight t)

(provide 'editor.rc)
;;; editor.rc.el ends here
