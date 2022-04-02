;;; window-management.rc.el --- Configuration related to managing windows  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Vojtech Stepancik

;; Author: Vojtech Stepancik <adalbert@AdalbertDEV>
;; Keywords: frames

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

(setq pop-up-frames 'graphic-only)
(setq frame-auto-hide-function #'delete-frame)

;; (advice-add 'set-window-dedicated-p :override #'ignore)

;; I honestly don't have much of an idea of what I'm doing here
(setq display-buffer-alist
      '(("\\*python-bg\\*" display-buffer-no-window
         (allow-no-window . t))
        ("\\*Org PDF LaTeX Output\\*" display-buffer-no-window
         (allow-no-window . t))
        (".*" ()
         (reusable-frames . t))))

(provide 'window-management.rc)
;;; window-management.rc.el ends here
