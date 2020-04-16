;;; magit.rc.el --- Configuration for magit

;;; Commentary:
;; 

;;; Code:

(use-package magit
  :straight t
  :custom
  (magit-save-repository-buffers nil))

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

(use-package transient
  :config
  (transient-bind-q-to-quit))

(provide 'magit.rc)

;;; magit.rc.el ends here
