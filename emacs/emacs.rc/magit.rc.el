;;; magit.rc.el --- Configuration for magit  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package magit
  :straight t
  :custom
  (magit-save-repository-buffers nil)
  (magit-commit-show-diff nil)
  (magit-bury-buffer-function #'delete-frame)
  (evil-magit-want-horizontal-movement t)
  :config
  (require 'evil-collection)
  (evil-collection-magit-setup))

(use-package forge
  :straight t
  :hook
  (magit-status-mode . (lambda ()
                         (require 'forge)))
  :config
  (cl-pushnew '("gitlab.fel.cvut.cz"
                "gitlab.fel.cvut.cz/api/v4"
                "gitlab.fel.cvut.cz"
                forge-gitlab-repository)
              forge-alist))

(use-package libgit
  :straight t)

(use-package magit-libgit
  :straight t
  :after (magit libgit))

(use-package magit-todos
  :straight t
  :after magit
  :hook
  (magit-mode . magit-todos-mode)
  :config
  (require 'evil-collection)
  (evil-collection-magit-todos-setup))

;; (use-package magit-delta
;;   :straight t
;;   :hook
;;   (magit-mode . magit-delta-mode))


(use-package transient
  :config
  (transient-bind-q-to-quit))

(provide 'magit.rc)

;;; magit.rc.el ends here
