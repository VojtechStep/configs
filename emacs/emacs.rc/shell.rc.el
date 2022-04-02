;;; shell.rc.el --- Configuration for Shell files (sh, fish)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Vojtech Stepancik

;; Author: Vojtech Stepancik <adalbert@AdalbertDEV>
;; Keywords: files, languages, unix

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'vs-utils.rc)
(require 'mode-local)

(use-package sh-script
  :init
  (defvaralias 'sh-basic-offset 'tab-width))

(use-package fish-mode
  :straight t
  :init
  (defvaralias 'fish-indent-offset 'tab-width))

(use-package esh-autosuggest
  :straight t
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package fish-completion
  :straight (:protocol https)
  :hook (eshell-mode . fish-completion-mode))

(use-package eshell
  :custom
  (eshell-prompt-function #'vs/--eshell-prompt)
  (eshell-prompt-regexp "^[^>\n]*> ")
  :hook
  (eshell-mode . (lambda ()
                   (setq truncate-lines nil)))
  :init
  (defmacro vs/--eshell-segment (form fg &optional bg &rest props)
    (declare (indent defun))
    (unless bg
      (setq bg ''term-color-black))
    (list 'propertize
          form
          ''face
          (append
           `(list :background (face-background ,bg)
                  :foreground (face-foreground ,fg))
           props)))
  (defun vs/--eshell-prompt ()
    (concat
     (vs/--eshell-segment (user-login-name)
       'term-color-green
       'term-color-black)
     (vs/--eshell-segment "@"
       'term-color-white)
     (vs/--eshell-segment (system-name)
       'term-color-white)
     " "
     (vs/--eshell-segment (vs/fishy-abbrev (abbreviate-file-name (eshell/pwd)))
       'term-color-green)
     (when-let ((branch (and (fboundp 'magit-get-current-branch)
                             (magit-get-current-branch))))
       (vs/--eshell-segment (format " (%s)" branch)
         'term-color-white))
     (vs/--eshell-segment "> "
       'term-color-white)))

  (defun eshell/j (&rest args)
    (setq args (flatten-tree args))
    (cond
     ;; No arguments -> home
     ((null args) (eshell/cd))
     ;; One argument "-" -> previous dir
     ((and (null (cdr args))
           (equal (car args) "-"))
      (eshell/cd "-"))
     ((and (null (cdr args))
           (file-directory-p (expand-file-name
                              (car args)
                              default-directory)))
      (eshell/cd (car args)))
     (t (eshell/cd (vs/zoxide-query (string-join args " "))))))

  (defun eshell/v (&rest args)
    "I'm so sorry."
    (setq args (flatten-tree args))
    (mapcar
     #'find-file
     args))

  :config
  (require 'evil-collection)
  (evil-collection-eshell-setup))

(use-package em-alias
  :after eshell
  :config
  (eshell/alias "ls" "exa -l --color=always $*"))

(use-package esh-help
  :straight t
  :demand t
  :after eshell
  :config
  (setup-esh-help-eldoc))

(provide 'shell.rc)
;;; shell.rc.el ends here
