;;; modeline.rc.el --- Configuration for modeline    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Vojtech Stepancik

;; Author: Vojtech Stepancik <vojtechstepancik@outlook.com>
;; Keywords: convenience

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

(use-package vs-modeline
  :straight (vs-modeline :type git
                         :host github
                         :repo "VojtechStep/vs-modeline.el"
                         :local-repo "~/Code/VojtechStep/Projects/vs-modeline.el")
  :demand t
  :config
  (vs-modeline-mode))

(provide 'modeline.rc)
;;; modeline.rc.el ends here
