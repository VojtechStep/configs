;;; language.rc.el --- Input method configuration    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Vojtech Stepancik

;; Author: Vojtech Stepancik <vojtechstepancik@outlook.com>
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

(use-package mozc
  :load-path "/usr/share/emacs/site-lisp/mozc"
  :functions (mozc-mode)
  :custom
  (mozc-candidate-style 'echo-area)
  (default-input-method "japanese-mozc")
  (mozc-leim-title "Aあ")
  :init
  (require 'mozc-autoloads)
  :config
  (when (and (fboundp 'native-compile-async)
             (not (subr-native-elisp-p (symbol-function #'mozc-mode))))
    (require 'find-func)
    (native-compile-async (find-library-name "mozc") nil t)))

(use-package ispell
  :custom
  (ispell-cmd-args
   (list "--conf-dir"
         "~/.config/aspell"
         "--data-dir"
         "~/.local/share/aspell"
         "--local-data-dir"
         "~/.local/share/aspell"
         "--home-dir"
         "~/.local/share/aspell")))

(provide 'language.rc)
;;; language.rc.el ends here
