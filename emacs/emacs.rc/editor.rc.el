;;; editor.rc.el --- Editor config  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; Builtins

(eval-when-compile
  (require 'use-package)
  (require 'vs-utils.rc))

(eval-and-compile
  (require 'mode-local))

(setq x-stretch-cursor t)

(use-package tooltip
  :config
  (tooltip-mode -1))

;; Consider CamelCase and snake_case as different words
(use-package subword
  :demand
  :config
  (global-subword-mode))

(use-package display-line-numbers
  :demand
  :custom
  (display-line-numbers-width-start t)
  (display-line-numbers-type 'visual)
  :hook
  ((prog-mode text-mode) . display-line-numbers-mode))

;; Highlight matching paranthesis
(use-package paren
  :demand
  :after theme
  :custom
  (show-paren-delay 0)
  ;; :hook
  ;; (prog-mode . (lambda ()
  ;;                (setq-local show-trailing-whitespace t)))
  :config
  (setq-mode-local prog-mode
                   show-trailing-whitespace t)
  (vs/run-with-frontend
   (require 'color)
   (dolist (desc `((show-paren-match
                    :foreground
                    ,(face-attribute 'cursor :background)
                    ;; :background nil
                    :background
                    ,(let ((bg (face-attribute 'default :background)))
                       (if (color-defined-p bg)
                           (color-lighten-name
                            (color-darken-name
                             (face-attribute 'default :background)
                             20)
                            1)
                         bg))
                    ;; :box (:line-width nil)
                    :slant italic)
                   (show-paren-mismatch
                    :foreground nil)
                   (trailing-whitespace
                    :background "red1")))
     (let ((face (car desc))
           (spec `((t . ,(cdr desc)))))
       (put face 'customized-face spec)
       (face-spec-set face spec))))
  (show-paren-mode))

(use-package simple
  :demand
  :custom
  (truncate-lines t))

(use-package debug
  :config
  (require 'evil-collection)
  (evil-collection-debug-setup))

(use-package elec-pair
  :demand
  :config
  (electric-pair-mode))

(use-package autoinsert
  :demand
  :config
  (auto-insert-mode))

;; External

(defvar vs/--loaded-theme nil)
(use-package color-theme-sanityinc-tomorrow
  :straight t
  :if (not vs/--loaded-theme)
  :demand
  :config
  (load-theme 'sanityinc-tomorrow-night t)
  (provide 'theme))

(use-package which-key
  :straight t
  :defer 1
  :custom
  (which-key-idle-delay 0.6)
  :config
  (which-key-mode))

(use-package activity-watch-mode
  :straight t
  :defer 1
  :config
  (global-activity-watch-mode))

(use-package hl-todo
  :straight t
  :defer 3
  :config
  (global-hl-todo-mode))

(use-package helpful
  :straight t
  :custom
  (helpful-max-buffers 1)
  :config
  (when (require 'evil-collection nil t)
    (evil-collection-helpful-setup)))

(use-package esup
  :straight t)

(use-package direnv
  :straight t
  :demand
  :config
  (require 'diff-mode)
  (direnv-mode))

(use-package shr
  :init
  (defun vs/--shr-put-image-always-slice (original spec alt &optional flags)
    (unless (assq 'size flags)
      (setq flags (cons '(size . original) flags)))
    (funcall original spec alt flags))
  :config
  (advice-add #'shr-put-image :around #'vs/--shr-put-image-always-slice))

(use-package calc
  :config
  (require 'evil-collection)
  (evil-collection-calc-setup))

(use-package shr-tag-pre-highlight
  :straight t
  :after shr
  :demand
  :init
  (defun vs/--render-pre (pre)
    "Internal function to render a PRE element.

Use `shr-tag-pre-highlight', but also make the background color lighter."
    (require 'org-faces)
    (let ((beg (point))
          ov)
      (shr-tag-pre-highlight pre)
      (setq ov (make-overlay beg (point)))
      (overlay-put ov 'face 'org-block)))
  :config
  (cl-pushnew '(pre . vs/--render-pre)
              shr-external-rendering-functions
              :test #'equal)
  (dolist (pair '(("nix" . nix)
                  ("haskell" . haskell)
                  ("hl" . haskell)))
    (cl-pushnew pair shr-tag-pre-highlight-lang-modes
                :test #'equal)))

(use-package info
  :config
  (when (require 'evil-collection nil t)
    (evil-collection-info-setup)))

(use-package stopwatch
  :straight (stopwatch
             :type git
             :host github
             :repo "blue0513/stopwatch"))

(use-package tabulated-list
  :config
  (require 'evil-collection)
  (evil-collection-tabulated-list-setup))

(provide 'editor.rc)
;;; editor.rc.el ends here
