;;; straight.rc --- Configuration for straight.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(eval-and-compile
  (require 'xdg))

(defvar straight-check-for-modifications '(find-when-checking check-on-save))
(defvar straight-repository-branch "develop")
(defvar straight-vc-git-default-protocol 'ssh)
(defvar straight-vc-git-default-clone-depth 1)

(defvar vs/cache-dir (expand-file-name "emacs" (xdg-cache-home)))

(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" vs/cache-dir)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(when (fboundp 'straight-use-package)
  (straight-use-package 'use-package))

(provide 'straight.rc)
;;; straight.rc.el ends here
