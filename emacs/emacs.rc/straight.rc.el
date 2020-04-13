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

(defvar vs/straight-host-alist '(("github.com" "gh")
                              ("gitlab.fel.cvut.cz" "gl")))

(defun vs/--straight-url-host->short (url)
  "Replace long host forms in URL with short ones according to `vs/straight-host-alist'."
  (cl-loop for (long short) in vs/straight-host-alist do
           (setq url (replace-regexp-in-string (concat "git@" long) (concat "git@" short) url)))
  url)
(when (fboundp 'straight-vc-git--encode-url)
  (advice-add #'straight-vc-git--encode-url :filter-return #'vs/--straight-url-host->short))

(defun vs/--straight-url-short->host (url)
  "Replace short host forms in URL with long ones according to `vs/straight-host-alist'."
  (setq url (car url))
  (cl-loop for (long short) in vs/straight-host-alist do
           (setq url (replace-regexp-in-string (concat "git@" short) (concat "git@" long) url)))
  (list url))
(when (fboundp 'straight-vc-git--decode-url)
  (advice-add #'straight-vc-git--decode-url :filter-args #'vs/--straight-url-short->host))

(when (fboundp 'straight-use-package)
  (straight-use-package 'use-package))

(provide 'straight.rc)
;;; straight.rc.el ends here
