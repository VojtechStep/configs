;;; env.rc.el --- Environment configuration          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Vojtech Stepancik

;; Author: Vojtech Stepancik <vojtech.stepancik.2e@stu.hosei.ac.jp>
;; Keywords: convenience, data

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

(use-package exec-path-from-shell
  :straight t
  :demand
  :custom
  ;; We set default Emacs shell to /bin/sh, but we want the variables from default shell
  (exec-path-from-shell-shell-name (getenv "SHELL"))
  (exec-path-from-shell-arguments nil)
  (exec-path-from-shell-variables
   '("PATH"
     "MANPATH"
     "DISPLAY"
     "CXX"
     "CC"
     "XDG_CONFIG_HOME"
     "XDG_CACHE_HOME"
     "XDG_DATA_HOME"
     "XAUTHORITY"
     "GNUPGHOME"
     "DOTFILES_HOME"
     "CARGO_HOME"
     "RUSTUP_HOME"
     "STACK_ROOT"
     "DOCKER_CONFIG"
     "TERMINFO"
     "SCREENSHOT_DIR"
     "PYTHONSTARTUP"
     "NPM_CONFIG_USERCONFIG"
     "NVM_DIR"
     "BROWSER"
     "FZF_DEFAULT_COMMAND"
     "FZF_DEFAULT_OPTS"
     "LESSHISTFILE"
     "LESS"
     "DOTNET_CLI_TELEMETRY_OPTOUT"
     "EMAIL"
     "NIX_PATH"))
  :config
  (exec-path-from-shell-initialize))

(provide 'env.rc)
;;; env.rc.el ends here
