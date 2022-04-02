;;; early-init.el --- Configuration loaded before rendering -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'cl-extra)
(require 'xdg)

(setq load-prefer-newer t
      shell-file-name "/bin/sh"
      create-lockfiles nil)

(defvar vs/--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disable GC during startup
(defconst vs/--gc-cons-threshold (* 100 1024 1024))
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(defun vs/--startup-cleanup ()
  "Reset settings disabled for faster startup."
  (setq file-name-handler-alist vs/--file-name-handler-alist)
  (setq gc-cons-threshold vs/--gc-cons-threshold)
  (setq gc-cons-percentage 0.1))
(add-hook 'emacs-startup-hook #'vs/--startup-cleanup)

(defun vs/--gc-enable ()
  "Enable garbage collection."
  (setq gc-cons-threshold vs/--gc-cons-threshold))
(defun vs/--defer-gc-enable (&rest _)
  "Enable garbage collection, defered."
  (run-at-time 1 nil #'vs/--gc-enable))
(add-hook 'minibuffer-exit-hook #'vs/--defer-gc-enable)
(add-hook 'company-completion-finished-hook #'vs/--defer-gc-enable)

(defun vs/--gc-disable (&rest _)
  "Disable garbage collection."
  (setq gc-cons-threshold most-positive-fixnum))
(add-hook 'minibuffer-setup-hook #'vs/--gc-disable)
(add-hook 'company-completion-started-hook #'vs/--gc-disable)

;; Load environment variables
;; (load "~/.config/envs.el")

;; Add config files to path
(push (expand-file-name "emacs.rc" user-emacs-directory) load-path)

;; Enable fuzzy matching
;; (push 'flex completion-styles)

;; Disable UI stuff as soon as possible
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(font . "JetBrains Mono") default-frame-alist)
(push '(background-color . "#1d1f21") default-frame-alist)
;; (push '(left-fringe . 32) default-frame-alist)
;; (push '(right-fringe . 32) default-frame-alist)


;; Disable blinking cursor
(setq no-blinking-cursor t
      blink-cursor-mode nil)

;; Probably can live without
;; (setq-default display-line-numbers 'visual)

;; No start screen
(setq inhibit-startup-screen t
      inhibit-compacting-font-caches t)

;; Default font is JetBrains Mono
(set-face-attribute 'default nil :height 130)

;; Sure, I know what I'm doing
(setq disabled-command-function nil)

;; If I feel like typing a shorter command,
;; I will put it in a keybinding
(setq suggest-key-bindings nil)

;; Vim like smooth scroll
(setq-default scroll-step 1)
(setq-default scroll-margin 3)
(setq-default auto-window-vscroll nil)
(setq-default scroll-conservatively 101)

;; Font rendering performance tips
(setq-default font-lock-support-mode 'jit-lock-mode)
(setq-default font-lock-multiline t)

;; LSP is very talkative
(setq-default read-process-output-max (* 1024 1024))

;; Extend the frame when told by the window manager
(setq-default frame-resize-pixelwise t)

(setq frame-inhibit-implied-resize t)

(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

(advice-add #'x-apply-session-resources :override #'ignore)

;; Only ask for a y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; The default is 8. 'Nuff said
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(setq-default indent-tabs-mode nil)

;; We aren't using typewriters any more
(setq-default sentence-end-double-space nil)

;; Configure user information
(setq user-mail-address "vojtechstepancik@outlook.com")
(setq user-full-name "Vojtech Stepancik")

(setq vc-handled-backends nil)
;; (setq vc-follow-symlinks t)

;; Prevent cluttering of the emacs config directory
(defconst vs/cache-dir (expand-file-name "emacs" (xdg-cache-home))
  "Where to store semi-persistent files.")
(cl-flet ((cache (lambda (name)
                   (expand-file-name name vs/cache-dir))))
  (with-no-warnings
    (setq auto-save-default nil)
    (setq backup-directory-alist `(("." . ,(cache "saves/"))))
    (setq bookmark-default-file (cache "bookmarks"))
    (setq custom-file (cache "custom.el"))
    (setq elfeed-db-directory (cache "elfeed"))
    (setq eshell-directory-name (cache "eshell"))
    (setq forge-database-file (cache "forge-database.sqlite"))
    (setq lsp-session-file (cache "lsp-session-v1"))
    (setq omnisharp-cache-directory (cache "omnisharp"))
    (setq org-id-locations-file (cache "org-id-locations"))
    (setq org-preview-latex-image-directory (cache "ltximg/"))
    (setq org-clock-persist-file (cache "org-clock-save.el"))
    (setq persist--directory-location (cache "persist"))
    (setq prescient-save-file (cache "prescient-save.el"))
    (setq projectile-cache-file (cache "projectile.cache"))
    (setq projectile-known-projects-file (cache "projectile-bookmarks.eld"))
    (setq request-storage-directory (cache "request/"))
    (setq straight-base-dir (cache ""))
    (setq sx-cache-directory (cache "sx"))
    (setq tramp-persistency-file-name (cache "tramp"))
    (setq transient-values-file (cache "transient/values.el"))
    (setq transient-history-file (cache "transient/history.el"))
    (setq transient-levels-file (cache "transient/levels.el"))
    (setq url-configuration-directory (cache "url/"))))

;; Disable package.el
(setq package-enable-at-startup nil)

;; Predefine use-package config
(defvar use-package-compute-statistics t)
(defvar use-package-always-defer t)
(defvar use-package-verbose nil)
(defvar use-package-use-theme nil)
(defvar use-package-expand-minimally (not use-package-verbose))

(defun vs/--extra-agenda-files-safe (v)
  "Check if a variable V is a string or a list."
  (or (stringp v)
      (and (listp v)
           (cl-every #'stringp v))))
(defvar vs/extra-agenda-files nil
  "Extra files to look for in `org-agenda'. Buffer local.")
(put 'vs/extra-agenda-files 'safe-local-variable #'vs/--extra-agenda-files-safe)

;; Try to load the theme
(defvar vs/--loaded-theme nil
  "Indicates whether the theme was loaded during early init.")

;; Worth a shot
;; (condition-case nil
;;     (progn
;;       (load (expand-file-name (concat "straight/repos/"
;;                                       "color-theme-sanityinc-tomorrow/"
;;                                       "color-theme-sanityinc-tomorrow.el")
;;                               vs/cache-dir))
;;       (setq vs/--loaded-theme
;;             (load-theme 'sanityinc-tomorrow-night t)))
;;   (error nil))

(provide 'early-init)
;;; early-init.el ends here
