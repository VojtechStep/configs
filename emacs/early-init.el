;;; early-init.el --- Configuration loaded before rendering -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; Load environment variables
(load "~/.envs.el")

;; Add config files to path
(cl-pushnew (expand-file-name "emacs.rc" user-emacs-directory) load-path)

;; Enable fuzzy matching
(cl-pushnew 'flex completion-styles)

;; Disable UI stuff as soon as possible
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Disable blinking cursor
(setq no-blinking-cursor t)

;; Probably can live without
(setq-default display-line-numbers nil)

;; No start screen
(setq inhibit-startup-screen t)

;; Load font configuration
(require 'fonts.rc)

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

;; LSP communication generates a lot of garbage
(setq-default gc-cons-threshold (* 100 1000 1000))
(setq-default read-process-output-max (* 1024 1024))

;; Extend the frame when told by the window manager
(setq-default frame-resize-pixelwise t)

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

;; Prevent cluttering of the emacs config directory
(defconst vs/cache-dir "~/.cache/emacs"
  "Where to store semi-persistent files.")
(cl-flet ((cache (lambda (name)
                   (expand-file-name name vs/cache-dir))))
  (with-no-warnings
    (setq backup-directory-alist `(("." . ,(cache "saves/"))))
    (setq custom-file (cache "custom.el"))
    (setq forge-database-file (cache "forge-database.sqlite"))
    (setq projectile-cache-file (cache "projectile.cache"))
    (setq projectile-known-projects-file (cache "projectile-bookmarks.eld"))
    (setq request-storage-directory (cache "request/"))
    (setq straight-base-dir (cache ""))
    (setq sx-cache-directory (cache "sx"))
    (setq transient-values-file (cache "transient/values.el"))
    (setq transient-history-file (cache "transient/history.el"))
    (setq transient-levels-file (cache "transient/levels.el"))
    (setq url-configuration-directory (cache "url/"))))
(setq auto-save-list-file-name nil)

;; Disable package.el
(setq package-enable-at-startup nil)

;; Predefine use-package config
(defvar use-package-verbose t)
(defvar use-package-compute-statistics t)
(defvar use-package-always-defer t)

;; Configure profiler
(defvar esup-depth 2)

;;
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
(condition-case nil
    (progn
      (load (expand-file-name (concat "straight/repos/"
                                      "color-theme-sanityinc-tomorrow/"
                                      "color-theme-sanityinc-tomorrow.el")
                              vs/cache-dir))
      (setq vs/--loaded-theme
            (load-theme 'sanityinc-tomorrow-night t)))
  (error nil))

(provide 'early-init)

;;; early-init.el ends here
