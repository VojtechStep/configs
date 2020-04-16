;;; straight.rc --- Configuration for straight.el

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defvar straight-check-for-modifications '(find-when-checking check-on-save))
(defvar straight-vc-git-default-protocol 'ssh)
(defvar straight-vc-git-default-clone-depth 1)

(defvar vs/cache-dir)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" vs/cache-dir))
      (bootstrap-version 5))
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
