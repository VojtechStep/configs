;;; mail.rc.el --- Configuration for the notmuch email client  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands mu4e
  :hook
  (mu4e-compose-mode . company-mode)
  (mu4e-compose-mode . flyspell-mode)
  (mu4e-context-changed . vs/mu4e-set-sender-from-context)
  :custom
  (mu4e-completing-read-function #'completing-read)
  (mu4e-get-mail-command "mbsync-par")
  (mu4e-confirm-quit nil)
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-images t)
  (mu4e-view-prefer-html t)
  (mu4e-view-show-addresses t)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-headers-seen-mark '("S" . ""))
  (mu4e-headers-new-mark '("N" . ""))
  (mu4e-headers-passed-mark '("P" . ""))
  (mu4e-headers-replied-mark '("R" . ""))
  (mu4e-headers-signed-mark '("s" . ""))
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'ask)
  (mu4e-compose-format-flowed t)
  (mu4e-change-filenames-when-moving)
  (mu4e-view-use-gnus nil)
  (mail-user-agent 'mu4e-user-agent)
  :init
  (defmacro vs/--mu4e-make-context (name email &rest vars)
    `(make-mu4e-context
      :name ,name
      :enter-func (lambda () (mu4e-message ,(format "Entering %s context" name)))
      :leave-func (lambda () (mu4e-message ,(format "Leaving %s context" name)))
      :match-func (vs/--maildir-in ,name)
      :vars '((user-mail-address . ,email)
              (mu4e-sent-messages-behavior . delete)
              . ,vars)))
  (defun vs/--maildir-in (name)
    (lambda (msg)
      (when msg
        (string-match-p
         (rx bol "/" (literal name) "/")
         (mu4e-message-field msg :maildir)))))
  (defun vs/mu4e-set-sender-from-context ()
    (when (and (eq mu4e-compose-type 'new)
               (eq major-mode 'mu4e-compose-mode)) ; Prevent error after asking to switch context when writing a new message
      (message "%s %s" (point-min) (point-max))
      (message-replace-header "From" (mu4e~draft-from-construct) nil t)))
  :config
  (plist-put (alist-get 'something mu4e-marks) :char '("*" . "✨"))
  (cl-pushnew '("View in browser" . mu4e-action-view-in-browser) mu4e-view-actions)
  (setq mu4e-contexts '()) ; removed for privacy
  (require 'evil-collection)
  (evil-collection-mu4e-setup)
  (dolist (s '(mu4e-main-mode-map
               mu4e-headers-mode-map
               mu4e-view-mode-map))
    (evil-collection-inhibit-insert-state s)))

(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"
      message-sendmail-f-is-evil t
      message-sendmail-envelope-from 'header
      message-sendmail-extra-arguments '("--read-envelope-from" "--read-recipients"))


(provide 'mail.rc)

;;; mail.rc.el ends here
