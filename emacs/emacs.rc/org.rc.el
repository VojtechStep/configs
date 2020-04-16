;;; org.rc.el --- Configuration for Org mode         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Vojtech Stepancik

;; Author: Vojtech Stepancik <vojtechstepancik@outlook.com>
;; Keywords: convenience, extensions, tools

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

(use-package org
  :straight (org :host github
                 :repo "emacs-straight/org-mode"
                 :local-repo "org")
  :hook
  (org-mode . org-indent-mode)
  :custom
  (org-startup-folded 'content)
  :config
  (defun vs/--org-agenda-add-local-files (files)
    (cond
     ((stringp vs/extra-agenda-files) (cl-pushnew vs/extra-agenda-files files))
     ((listp vs/extra-agenda-files) (append vs/extra-agenda-files files))
     (t files)))
  (advice-add #'org-agenda-files :filter-return #'vs/--org-agenda-add-local-files))

(provide 'org.rc)
;;; org.rc.el ends here
