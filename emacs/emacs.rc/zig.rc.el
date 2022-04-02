;;; zig.rc.el --- Configuration for Zig              -*- lexical-binding: t; -*-

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

(use-package zig-mode
  :straight t
  :custom
  (zig-format-on-save nil)
  :hook
  (zig-mode . (lambda ()
                (apheleia-mode)
                (auto-fill-mode -1)
                (lsp-deferred)))
  :config
  (when (require 'apheleia nil t)
    (cl-pushnew '(zig-fmt "zig" "fmt" "--stdin") apheleia-formatters)
    (cl-pushnew '(zig-mode . zig-fmt) apheleia-mode-alist))
  (when (require 'lsp-mode nil t)
    (cl-pushnew '(zig-mode . "zig") lsp-language-id-configuration)
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "zls")
      :major-modes '(zig-mode)
      :server-id 'zls))))

(provide 'zig.rc)
;;; zig.rc.el ends here
