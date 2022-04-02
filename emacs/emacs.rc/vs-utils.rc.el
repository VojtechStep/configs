;;; vs-utils.rc --- Various utilities specific to my setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'seq))

(defconst vs/project-dir "~/Code/VojtechStep/Projects"
  "Directory from which to load local projects.")

(defun vs/projectile-rg ()
  "Run a search with ripgrep on the current project."
  (interactive)
  (rg-project (rg-read-pattern nil) "all"))

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
    (if (and thing (symbolp thing))
        (describe-symbol thing)
      (woman))))

(defun vs/ff-dwim (other-window)
  "Find file DWIM, if OTHER-WINDOW is specified, use other window.
If in a projectile project, open file explorer, if not, open regular ff."
  (interactive (list nil))
  (if (and
       (require 'projectile nil 'noerror)
       (projectile-project-p))
      (call-interactively (if other-window
                              #'projectile-find-file-other-window
                            #'projectile-find-file))
    (call-interactively (if other-window
                            #'find-file-other-window
                          #'find-file))))

(defun vs/popup-buffer (&optional buffer)
  "Popup BUFFER in other frame."
  (interactive)
  (display-buffer-other-frame (or buffer (current-buffer)))
  (kill-buffer buffer))

(defun vs/ff-dwim-other-window ()
  "Same as `vs/ff-dwim', but force the OTHER-WINDOW flag."
  (interactive)
  (vs/ff-dwim t))

(defun vs/helm-switch-to-projects ()
  "Close current helm session and open the project selection."
  (interactive)
  (with-helm-alive-p
    (helm-run-after-exit #'helm-projectile-switch-project)))

(cl-defun vs/--general-parse-binding (prefix key (doc cmd . rest))
  "Parse a binding specification starting with PREFIX, then KEY, settings `which-key' hints to DOC and launching BINDING."
  (require 'seq)
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

(defun vs/window-abs-offset (&optional window)
  "Get the possition of WINDOW in pixels.
The position is relative to the top left corner of the frame."
  (let ((start-win (or window (selected-window))))
    (cons
     (cl-do ((win (window-in-direction 'left start-win t)
                  (window-in-direction 'left win t))
             (xoff 0 (+ xoff (window-pixel-width win))))
         ((null win) xoff))
     (cl-do ((win (window-in-direction 'above start-win t)
                  (window-in-direction 'above win t))
             (yoff 0 (+ yoff (window-pixel-height win))))
         ((null win) yoff))
     )))

;; TODO: For some reason, outputing to string freezes
;; (defvar vs/--last-screenshot-name nil
;;   "Name of the last screenshot taken.")

(defun vs/make-scrot ()
  "Take a screenshot of region."
  (interactive)
  (let* ((beg (region-beginning))
         (voff (cdr (window-text-pixel-size
                     nil
                     (window-start)
                     beg
                     0)))
         (window-abs (vs/window-abs-offset))
         (lfringe (frame-parameter nil 'left-fringe))
         (dims (window-text-pixel-size
                nil
                beg
                (region-end))))
    (internal-show-cursor nil nil)
    (set-mark nil)
    (redisplay)
    (with-demoted-errors "Error taking codeshot: %S"
      (call-process
       "codeshot.sh"
       nil
       "*Messages*"
       nil
       (frame-parameter nil 'window-id)
       (number-to-string (+ lfringe (car window-abs)))
       (number-to-string (+ voff (cdr window-abs)))
       (number-to-string (car dims))
       (number-to-string (cdr dims))))
    (internal-show-cursor nil t)
    (redisplay)))

(defvar vs/first-input-hook nil)
(put 'vs/first-input-hook 'permanent-local t)
(defun vs/--first-input-hook-oneshot (&rest _)
  "Fire first input hook, then unhook it."
  (run-hooks 'vs/first-input-hook)
  (set 'vs/first-input-hook nil)
  (remove-hook 'pre-command-hook #'vs/--first-input-hook-oneshot))
(add-hook 'pre-command-hook #'vs/--first-input-hook-oneshot)

(defun vs/japanese-lookup (&optional word)
  "Search WORD or selection on jisho.org."
  (interactive (let ((mark (mark)))
                 (list (if (use-region-p)
                           (prog1
                               (buffer-substring-no-properties mark (point))
                             (setq deactivate-mark t))
                         (buffer-substring-no-properties mark (1+ mark))))))
  (when word
    (browse-url (format "https://www.jisho.org/search/%s" word))))

(defun vs/kill-or-bury-buffer (&optional window)
  "Kill buffer shown in WINDOW if it's not displayed anywhere else.
Otherwise, bury the buffer."
  (interactive)
  (unless window
    (setq window (selected-window)))
  (let ((buffer (window-buffer window)))
    (cl-assert (bufferp buffer))
    (if (cl-loop for w in (get-buffer-window-list buffer 'never t)
                 thereis (not (eql w window)))
        (switch-to-prev-buffer window 'bury)
      (kill-buffer buffer))))

(defvar-local vs/immortal nil
  "If non-nil, prevent `vs/kill-all-buffers' from closing this buffer.")

(defun vs/kill-all-buffers ()
  "Kill all buffers that don't have a non-nil `vs/immortal'."
  (interactive)
  (cl-loop for b being the buffers
           unless (buffer-local-value 'vs/immortal b)
           do (kill-buffer b)))

(defun vs/add-project-maybe (dir)
  "Add DIR to list of projects if it points to a valid project."
  (and (require 'projectile nil t)
       (projectile-project-p dir)
       (projectile-add-known-project dir)))

(defun vs/current-project-or-prompt (&optional prompt)
  "Get the project directory.

Use the project of the current buffer.
If PROMPT is non-nil, prompt for a project."
  (if-let ((prompt)
           (projects (projectile-relevant-known-projects)))
      (projectile-completing-read
       "Scratch for project: " projects)
    (projectile-project-root)))

(defun vs/project-scratch-buffer-other-window (&optional root &rest _)
  "Open a scratch buffer for project in ROOT in other window.

Like `vs/project-scratch-buffer', but open in other window"
  (interactive (list (vs/current-project-or-prompt current-prefix-arg)))
  (vs/project-scratch-buffer root t))

(defun vs/project-scratch-buffer (&optional root other-window)
  "Open a scratch buffer for project in ROOT.

If OTHER-WINDOW is non-nil, open in a another window.

Process:
1. See if ROOT is the root of some project
2. If so, open a scratch file for the project
3. See if ROOT is contained in some project"
  (interactive (list
                (vs/current-project-or-prompt current-prefix-arg)
                nil))
  (require 'projectile)
  ;; is argument nil? -> set it to home
  ;; is the result a project? -> set it to project
  ;; -> set it to home
  (setq root (if (and root (file-directory-p root))
                 root
               "~/"))
  (if-let ((project (projectile-project-p root)))
      (setq root project))
  (setq root (abbreviate-file-name root))
  (let* ((projname (or (projectile-project-name root)
                       (file-name-base root)))
         (bufname (format "*scratch*<%s>" projname))
         (existing (get-buffer bufname))
         (show-fn (if other-window #'switch-to-buffer-other-window #'switch-to-buffer)))
    ;; TODO: Handle multiple projects with the same name
    (funcall show-fn
             (or existing
                 (with-current-buffer (generate-new-buffer bufname)
                   (cd root)
                   (insert (substitute-command-keys initial-scratch-message))
                   (if (fboundp initial-major-mode)
                       (funcall initial-major-mode))
                   (current-buffer))))
    (setq-local vs/immortal t)
    (current-buffer)))

(defmacro vs/run-with-frontend (&rest body)
  "Run BODY when there is a frontend.

Emacs is weird when starting as a server, ok?

Some things aren't available (like face definitions),
so you want to hook into `server-after-make-frame-hook'
\(which confusingly refers to terminal clients too),
but that doesn't fire when opening Emacs without a server...

This macro checks if a server is running, and if it is,
it adds BODY to the hook,
and removes it after the first client is created.

If a server is not running, which means that the current instance was launched
as a normal Emacs process, run BODY straight away."
  (let ((funcname (cl-gentemp "vs/--run-with-frontend-")))
    (macroexp-progn
     `((defun ,funcname ()
         ,@body
         (remove-hook 'server-after-make-frame-hook #',funcname))
       (if (daemonp)
           (add-hook 'server-after-make-frame-hook #',funcname)
         (,funcname))))))

(defvar vs/--mpv-play-history nil
  "History variable used for `vs/mpv-play'.")
(defun vs/mpv-play (url)
  "Play URL in mpv, or ask for one interactively."
  (interactive (list (read-string "URL: " nil 'vs/--mpv-play-history)))
  (when (and (stringp url)
             (< 0 (length url)))
    (let ((mpv-buffer (generate-new-buffer (generate-new-buffer-name " *mpv*"))))
      (start-process "mpv" mpv-buffer "mpv" url))))

(declare-function org-get-heading 'org)
(defun vs/yank-heading ()
  "Copy the text of the current heading."
  (interactive)
  (when (eq major-mode 'org-mode)
    (kill-new (substring-no-properties (org-get-heading t t t t)))))

(defun vs/--abbrev-terminal (filename)
  (or (equal filename "/")
      (equal filename "~")))
(defun vs/fishy-abbrev (filename)
  "Abbreviate FILENAME in a way similar to the fish shell.

That means replace `/home/<user>/' with `~/' and shorten every directory name
except for the last segment to just the first letter.

For example /home/bob/Code/Projects -> ~/C/Projects"
  (let ((dir filename)
        segments)
    (when (file-directory-p filename)
      (setq filename (directory-file-name filename)))
    (unless (vs/--abbrev-terminal filename)
      (push (file-name-nondirectory filename) segments)
      (setq dir (directory-file-name (file-name-directory filename))))
    (while (not (vs/--abbrev-terminal dir))
      (let ((parent (file-name-directory dir))
            (segment (file-name-nondirectory dir)))
        (push (substring segment 0 1) segments)
        (setq dir (directory-file-name parent))))
    (if (equal dir "/")
        (push "" segments)
      (push dir segments))
    (string-join segments "/")))

(defun vs/zoxide-query (query)
  "Call zoxide with QUERY."
    (unless (stringp query)
      (error "Query must be a string"))
    (let ((output (shell-command-to-string
                    (concat "zoxide query -- "
                            query))))
      (string-trim output)))

(defun vs/zoxide-register (directory)
  "Register a visit to DIRECTORY."
  (when (file-directory-p directory)
    (call-process "zoxide" nil nil nil "add" directory)))

(defun vs/shutdown (doit)
  "If DOIT is non-nil, perform system shutdown."
  (interactive (list (yes-or-no-p "Shut down? ")))
  (when doit
    (let ((confirm-kill-emacs #'ignore))
      (save-buffers-kill-emacs))
    (call-process "systemctl" nil 0 nil "poweroff")))

(provide 'vs-utils.rc)
;;; vs-utils.rc.el ends here
