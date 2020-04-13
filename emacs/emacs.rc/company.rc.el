;;; company.rc.el --- Configuration for company-mode

;;; Commentary:
;; 

;;; Code:

(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  (company-require-match nil)
  (company-frontends '(company-pseudo-tooltip-frontend))
  (company-backends '(company-capf
                      company-files
                      (company-dabbrev-code company-keywords)
                      company-dabbrev))
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-downcase nil)
  :config
  ;; Can't set a variable to an uninitialized value
  (setq company-tooltip-minimum company-tooltip-limit))

(provide 'company.rc)

;;; company.rc.el ends here
