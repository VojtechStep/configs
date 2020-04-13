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
  :config
  (defmacro forge-derive (newhost host)
            `(cl-pushnew (cons ,newhost
                               (cdr (seq-find (lambda (it)
                                                (string= (car it)
                                                         ,host))
                                              forge-alist)))
                         forge-alist))
  (forge-derive "gh" "github.com"))

(use-package transient
  :config
  (transient-bind-q-to-quit))

(provide 'magit.rc)

;;; magit.rc.el ends here
