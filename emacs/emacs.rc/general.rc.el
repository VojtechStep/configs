;;; general.rc.el --- Keybindigs configured with general.el

;;; Commentary:
;; 

;;; Code:

(defmacro vs/lcag (key)
  "Append LCtrl, Alt and GUI before KEY."
  `(concat "C-M-s-" ,key))

(use-package general
  :straight t
  :demand
  :config

  (require 'vs-utils.rc)
  (require 'newcomment)

;; Generic keybindings
  (general-def
    "M-x" #'helm-M-x
    "M-b" #'helm-buffers-list
    "C-f" #'vs/helm-projectile-rg
    "C-h a" #'helm-apropos
    "C-S-u" #'vs/input-unicode
    (vs/lcag "h") #'evil-window-left
    (vs/lcag "S-h") #'evil-window-move-far-left
    (vs/lcag "j") #'evil-window-down
    (vs/lcag "S-j") #'evil-window-move-very-bottom
    (vs/lcag "k") #'evil-window-up
    (vs/lcag "S-k") #'evil-window-move-very-top
    (vs/lcag "l") #'evil-window-right
    (vs/lcag "S-l") #'evil-window-move-far-right)

  (general-def 'motion 'override
    "j" #'evil-next-visual-line
    "k" #'evil-previous-visual-line
    "L" #'evil-end-of-line
    "H" #'evil-first-non-blank
    ";" #'evil-ex
    "m" #'evil-snipe-s
    "M" #'evil-snipe-S
    "C-;" #'comment-dwim)

  (general-unbind 'normal
    "m"
    "M")

  (general-def 'insert 'override
    "C-;" #'comment-dwim
    "C-<ret>" #'comment-indent-new-line
    "C-v" #'yank
    "C-S-v" #'quoted-insert
    "C-SPC" #'company-complete)
  
  (general-unbind 'motion
    "C-f"
    "SPC"
    "RET")

  (general-def 'visual 'override
    "<tab>" #'align)

  (vs/general-bind-tree 'normal 'override
    "SPC" ("leader" :sub
            "SPC" ("last buffer" evil-switch-to-windows-last-buffer)
            "M" ("magit" magit)
            "cc" ("close buffer" evil-delete-buffer)
            "f" ("files in project" vs/ff-dwim)
            "g" ("buffer control" :sub
                "g" ("buffer in project" helm-projectile-switch-to-buffer)
                "a" ("any buffer" helm-buffers-list))
            "p" ("project control" :sub
                "o" ("open project" helm-projectile-switch-project)
                "t" ("open vterm" projectile-run-vterm)
                "ka" ("close all buffers" projectile-kill-buffers)
                "c" ("compile project" projectile-compile-project)
                "x" ("project actions" projectile-commander))
            "e" ("errors" :sub
                "j" ("next" flycheck-next-error)
                "k" ("prev" flycheck-previous-error)
                "l" ("list" flycheck-list-errors))
            "j" ("lsp" nil)))

  ;; Mode specific keybindings
  ;; Package menu
  (general-def 'normal 'package-menu-mode-map
    "i" #'package-menu-mark-install
    "U" #'package-menu-mark-upgrades
    "d" #'package-menu-mark-delete
    "u" #'package-menu-mark-unmark
    "x" #'package-menu-execute
    "q" #'quit-window
    "gr" #'package-menu-refresh
    "g?" #'package-menu-describe-package)

  ;; Tables
  (general-def 'motion 'tabulated-list-mode-map
    "s" #'tabulated-list-sort)

  ;; Org
  (general-def 'normal 'org-mode-map
    "TAB" #'org-cycle)

  ;; Helm
  (general-def 'helm-map
    "C-j" #'helm-next-line
    "C-k" #'helm-previous-line
    "C-l" #'helm-execute-persistent-action)

  (general-def helm-find-files-map
    "C-p" #'vs/helm-switch-to-projects)

  ;; Company
  (general-def 'company-active-map
    "C-j" #'company-select-next
    "C-k" #'company-select-previous
    "C-t" #'company-show-doc-buffer
    "C-w" nil)

  (general-def 'normal 'haskell-mode-map
    "o" #'vs/--haskell-evil-open-below
    "O" #'vs/--haskell-evil-open-above)
    )

(defun vs/--haskell-evil-open-above ()
  "Add a line above and indent using haskell indentation."
  (interactive)
  (evil-beginning-of-line-or-digit-argument)
  (haskell-indentation-newline-and-indent)
  (evil-previous-line)
  (haskell-indentation-indent-line)
  (evil-append-line nil))

(defun vs/--haskell-evil-open-below ()
  "Add a line below and indent using haskell indentation."
  (interactive)
  (evil-append-line nil)
  (haskell-indentation-newline-and-indent))


(provide 'general.rc)

;;; general.rc.el ends here
