;;; csharp.rc.el --- Configuration for C#            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Vojtech Stepancik

;; Author: Vojtech Stepancik <adalbert@AdalbertDEV>
;; Keywords: languages

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

(use-package csharp-mode
  :straight t
  :hook
  (csharp-mode . vs/csharp-local-setup)
  :config
  (defun vs/csharp-local-setup ()
    (c-set-style "c#")))

(use-package omnisharp
  :straight t
  :disabled
  :custom
  (omnisharp-expected-server-version "1.35.2")
  :hook
  (csharp-mode . omnisharp-mode)
  :config
  (setq-mode-local csharp-mode
                   company-backends (cons 'company-omnisharp
                                          company-backends))
  (defun vs/restart-omnisharp-server ()
    (interactive)
    (omnisharp-stop-server)
    (omnisharp-start-omnisharp-server)))

(provide 'csharp.rc)
;;; csharp.rc.el ends here
