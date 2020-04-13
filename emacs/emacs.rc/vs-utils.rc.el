;;; vs-utils.rc --- Various utilities specific to my setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defconst vs/project-dir "~/Code/VojtechStep/Projects"
  "Directory from which to load local projects.")

(defun vs/helm-projectile-rg ()
  "Run a `helm-ag' interface with ripgrep in the projectile project."
  (interactive)
  (if (and (require 'projectile nil 'noerror)
           (require 'helm-ag nil 'noerror))
      (let ((projdir (projectile-project-root)))
        (if projdir
            (helm-do-ag projdir)
          (message "Not in a project")))
    (warn "This function requires projectile and helm-ag to work")))

(defvar vs/--input-unicode-keymap (make-sparse-keymap)
  "Keymap to be used when typing a unicode hexcode.
The primary purpose of this keymap is to end the prompt on SPC")
(define-key vs/--input-unicode-keymap (kbd "SPC") #'exit-minibuffer)
(define-key vs/--input-unicode-keymap (kbd "C-g") #'abort-recursive-edit)

(defun vs/input-unicode ()
  "Ask for a hexadecimal representation of a unicode char and insert it.
Bind to C-S-u for a GNOME-like unicode input experience."
  (interactive)
  (let ((hex (read-from-minibuffer "" nil vs/--input-unicode-keymap)))
    (self-insert-command 1 (string-to-number hex 16))))

(defun vs/help-dwim ()
  "Describe thing at point if it is a function or a bound symbol.
Open the manual otherwise."
  (interactive)
  (let ((thing (symbol-at-point)))
    (if (symbolp thing)
        (describe-symbol thing)
      (woman))))

(defun vs/ff-dwim ()
  "Find file DWIM.
If in a projectile project, open file explorer, if not, open helm ff."
  (interactive)
  (if (and
       (require 'projectile nil 'noerror)
       (projectile-project-p)
       (require 'helm-projectile nil 'noerror))
      (call-interactively #'helm-projectile-find-file)
    (call-interactively #'helm-find-files)))

(defun vs/helm-switch-to-projects ()
  "Close current helm session and open the project selection."
  (interactive)
  (with-helm-alive-p
    (helm-run-after-exit #'helm-projectile-switch-project)))

(cl-defun vs/--general-parse-binding (prefix key (doc cmd . rest))
  "Parse a binding specification starting with PREFIX, then KEY, settings `which-key' hints to DOC and launching BINDING."
  (setq prefix (concat prefix " " key))
  (if (eq cmd :sub)
      (apply
       #'append
       `(,prefix '(:ignore t :which-key ,doc))
       (mapcar (lambda (pair)
                 (apply #'vs/--general-parse-binding prefix pair))
               (seq-partition rest 2)))
    `(,prefix
      (list
       ,@(if cmd `(#',cmd) '(:ignore t))
       :which-key ,doc))))

(cl-defmacro vs/general-bind-tree (mode keymap &rest binding)
  "Define keybindings in evil MODE for KEYMAP with `general' based on BINDING."
  `(progn
    (require 'seq)
    (general-def ,mode ,keymap
    ,@(apply #'vs/--general-parse-binding "" binding))))

(defun vs/zip-current-file ()
  "Create a zip file with the currently open file as the contents.
Prompt for the archive name."
  (interactive)
  (let* ((buffer (current-buffer))
         (filename (buffer-name buffer))
         (file-ok (file-readable-p filename)) )
    (when file-ok
      (let ((archive-name (read-from-minibuffer "Arhive name: ")))
        (when archive-name
          (shell-command (format "zip \"%s\" \"%s\"" archive-name filename)))))))

(provide 'vs-utils.rc)
;;; vs-utils.rc.el ends here
