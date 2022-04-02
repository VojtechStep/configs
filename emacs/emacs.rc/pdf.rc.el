;;; pdf.rc.el --- Configuration for PDF viewer       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Vojtech Stepancik

;; Author: Vojtech Stepancik <adalbert@AdalbertDEV>
;; Keywords: files, multimedia

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
  (require 'use-package)
  (require 'mode-local))

(use-package pdf-tools
  :straight (:fork (:repo "flatwhatson/pdf-tools"
                    :branch "fix-macros"))
  :commands pdf-view-mode
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook
  (pdf-view-mode . auto-revert-mode)
  :custom
  (pdf-misc-print-program "lp")
  (pdf-util-convert-font "JetBrains-Mono-Regular")
  :config
  (setq-mode-local pdf-view-mode display-line-numbers nil)
  (defalias 'pdf-misc-print-program #'pdf-misc-print-programm)
  (require 'evil-collection)
  (evil-collection-pdf-setup))

;; For autoloads
(use-package pdf-links)

(provide 'pdf.rc)
;;; pdf.rc.el ends here
