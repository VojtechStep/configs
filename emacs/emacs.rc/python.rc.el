;;; python.rc.el --- Configuration for Python        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Vojtech Stepancik

;; Author: Vojtech Stepancik <vojtech.stepancik.2e@stu.hosei.ac.jp>
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

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package python-mode
  :custom
  (python-shell-interpreter "python")
  (python-indent-guess-indent-offset nil))

;; (use-package lsp-python-ms
;;   :straight t
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (setq lsp-python-ms-executable (executable-find "python-language-server"))
;;                          (lsp-deferred))))

(use-package lsp-pyright
  :straight t
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))

(provide 'python.rc)
;;; python.rc.el ends here
