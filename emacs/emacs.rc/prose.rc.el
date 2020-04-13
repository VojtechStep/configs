;;; prose.rc.el --- Configuration for modes that are not programming, but prose

;;; Commentary:
;; 

;;; Code:

(defgroup prose nil
  "Settings for prose mode."
  :group 'text
  :prefix "prose-")

(defface prose-default-face
  ;; '((t . (:family "Victor mono")))
  '((t . '()))
  "Face to be used in prose files."
  :group 'prose)

(defcustom prose-line-spacing nil
  "Line spacing to use in prose mode."
  :type 'any
  :group 'prose)

(defvar buffer-face-mode-face)

(defvar-local prose-prev-line-spacing line-spacing)

;;;###autoload
(define-minor-mode prose-mode
  "Mode for editing prose documents."
  :lighter " Prose"
  (require 'face-remap)
  (if prose-mode
      (progn
        (setq buffer-face-mode-face 'prose-default-face)
        (setq prose-prev-line-spacing line-spacing)
        (setq line-spacing prose-line-spacing)
        (set-face-attribute 'bold nil :weight 'extrabold))
    (setq line-spacing prose-prev-line-spacing))
  (buffer-face-mode (or prose-mode -1)))

(provide 'prose.rc)

;;; prose.rc.el ends here
