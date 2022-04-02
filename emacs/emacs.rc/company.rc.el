;;; company.rc.el --- Configuration for company-mode  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package company
  :straight t
  :hook
  ((prog-mode lisp-interaction-mode)
   . (lambda ()
       (when (or (eq major-mode 'lisp-interaction-mode)
                 (when-let ((file (buffer-file-name)))
                   (file-writable-p file)))
         (company-mode))))
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  (company-require-match nil)
  (company-frontends '(company-pseudo-tooltip-frontend))
  ;; (company-backends '(company-capf company-files))
  (company-backends '(company-files company-capf))
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-downcase nil)
  :config
  ;; Can't set a variable to an uninitialized value
  (setq company-tooltip-minimum company-tooltip-limit))

(use-package company-prescient
  :straight t
  :hook (company-mode . company-prescient-mode))

(provide 'company.rc)

;;; company.rc.el ends here
