;;; general.rc.el --- Keybindigs configured with general.el  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(defmacro vs/lcag (key)
  "Append LCtrl, Alt and GUI before KEY."
  `(concat "C-M-s-" ,key))

(use-package general
  :straight t
  :hook
  (vs/first-input . (lambda () (require 'general)))
  :config

  (require 'vs-utils.rc)
  (require 'language.rc)
  (require 'newcomment)
  (require 'evil)
  (require 'transient)

;; Generic keybindings
  (general-def 'override
    "C-f" #'consult-ripgrep
    "C-S-u" #'vs/input-unicode
    (vs/lcag "h") #'evil-window-left
    (vs/lcag "S-h") #'evil-window-move-far-left
    (vs/lcag "j") #'evil-window-down
    (vs/lcag "S-j") #'evil-window-move-very-bottom
    (vs/lcag "k") #'evil-window-up
    (vs/lcag "S-k") #'evil-window-move-very-top
    (vs/lcag "l") #'evil-window-right
    (vs/lcag "S-l") #'evil-window-move-far-right
    (vs/lcag "p") #'vs/make-scrot
    [remap org-capture] #'vs/org-capture
    [remap describe-function] #'helpful-callable
    [remap describe-variable] #'helpful-variable
    [remap describe-key] #'helpful-key
    [remap describe-symbol] #'helpful-symbol)

  (general-def 'evil-replace-state-map
    "C-S-u" #'vs/input-unicode)

  (general-unbind
    "M-;")

  (general-def 'motion
    "j" #'evil-next-visual-line
    "k" #'evil-previous-visual-line
    "L" #'evil-end-of-line-or-visual-line
    "H" #'evil-first-non-blank-of-visual-line
    ";" #'evil-ex
    "m" #'evil-snipe-s
    "M" #'evil-snipe-S
    "C-;" #'comment-dwim
    "C-a" #'evil-numbers/inc-at-pt
    "C-S-a" #'evil-numbers/dec-at-pt)

  (general-def 'minibuffer-local-map
    "C-v" #'yank
    "C-w" #'evil-delete-backward-word)

  (general-unbind 'normal
    "m"
    "M")

  (general-def 'insert 'override
    "C-;" #'comment-dwim
    ;; "M-<return>" #'comment-indent-new-line
    "C-v" #'yank
    "C-S-v" #'quoted-insert
    "C-SPC" #'company-complete
    "M-;" #'vs/--evil-normal-then-ex)

  (general-unbind 'motion
    "C-f"
    "SPC"
    "RET")

  (general-def 'visual 'override
    "<tab>" #'align
    "gl" #'count-words-region)

  (transient-define-prefix vs/transient-transmission ()
    ["Transmission"
     ("t" "feed" transmission)
     ("a" "add" transmission-add)])

  (transient-define-prefix vs/transient-apps ()
    ["Apps"
     ("m" "mail" mu4e)
     ("f" "RSS feed" elfeed)
     ("s" "eshell" eshell)
     ("d" "autojump" vs/dired-zoxide)
     ("i" "iRC" erc)
     ("w" "web" eww)
     ("b" "bluetooth" bluetooth-list-devices)
     ("y" "YouTube" ytdious)
     ("t" "transmission" vs/transient-transmission)
     ("v" "Video" vs/mpv-play)])

  (vs/general-bind-tree '(motion normal) 'override
    "SPC" ("leader" :sub
           "C-g" ("abort" keyboard-quit)
           "SPC" ("last buffer" evil-switch-to-windows-last-buffer)
           "m" ("magit" :sub
                "m" ("magit status" magit-status)
                "f" ("magit file" magit-file-dispatch))
           "c" ("close" :sub
                "c" ("close current buffer" bury-buffer)
                "k" ("kill current buffer" vs/kill-or-bury-buffer)
                "a" ("close all buffers" vs/kill-all-buffers))
           "f" ("files in project" vs/ff-dwim)
           "F" ("files in project other window" vs/ff-dwim-other-window)
           "g" ("buffer control" :sub
                "g" ("buffer in project" projectile-switch-to-buffer)
                "a" ("any buffer" consult-buffer)
                "o" ("open current buffer in new frame" vs/popup-buffer)
                "s" ("open project scratch buffer" vs/project-scratch-buffer)
                "S" ("open project scratch buffer in other window" vs/project-scratch-buffer-other-window)
                "R" ("rename buffer" rename-buffer)
                "r" ("revert buffer" revert-buffer)
                "c" ("compare buffer" diff-buffer-with-file))
           "p" ("project control" :sub
                "o" ("open project" projectile-switch-project)
                "t" ("open vterm" projectile-run-vterm)
                "ka" ("close all buffers" projectile-kill-buffers)
                "c" ("compile project" projectile-compile-project)
                "d" ("project dired" projectile-dired)
                "x" ("project actions" projectile-commander))
           "e" ("errors" :sub
                "j" ("next" flycheck-next-error)
                "k" ("prev" flycheck-previous-error)
                "l" ("list" flycheck-list-errors))
           "j" ("lsp" nil)
           "a" ("apps" vs/transient-apps)
           "o" ("org" :sub
                "a" ("agenda" vs/org-agenda)
                "f" ("agenda files" vs/org-open-agenda-file)
                "c" ("capture" org-capture)
                "q" ("cancel clock" org-clock-cancel)
                "g" ("goto clocking" org-clock-goto)
                "i" ("clock in" vs/org-clock-in)
                "o" ("clock out" vs/org-clock-out))))

  ;; Mode specific keybindings
  ;; Mail
  (general-def 'normal 'mu4e-main-mode-map
    "U" #'mu4e-update-index
    "gr" #'mu4e-update-mail-and-index)
  (general-def 'normal 'mu4e-headers-mode-map
    "C-c C-b" #'mu4e-headers-query-prev)

  ;; (general-def 'normal 'pdf-view-mode-map
  ;;   "j" #'pdf-continuous-scroll-forward
  ;;   "k" #'pdf-continuous-scroll-backward)

  ;; Org
  (general-def 'normal 'org-mode-map
    "<tab>" #'org-cycle)

  (general-def '(motion insert) 'org-mode-map
    [remap evil-delete-backward-char] #'evil-org-delete-backward-char
    [remap evil-delete-backward-char-and-join] #'evil-org-delete-backward-char
    [remap self-insert-command] #'org-self-insert-command)

  (general-def 'normal 'org-src-mode-map
    [remap evil-write] #'org-edit-src-save)

  (general-def 'motion 'org-agenda-mode-map
    "gf" #'vs/open-agenda-file)

  (general-def 'normal 'evil-org-mode-map
    "gx" #'org-open-at-point)

  ;; Images
  (general-def 'normal 'image-mode-map
    "+" #'image-increase-size
    "-" #'image-decrease-size
    "h" #'image-scroll-right
    "j" #'image-scroll-up
    "k" #'image-scroll-down
    "l" #'image-scroll-left)

  ;; Company
  (general-def 'company-active-map
    "C-j" #'company-select-next
    "C-k" #'company-select-previous
    "C-t" #'company-show-doc-buffer
    "C-w" nil)

  ;; Haskell
  (general-def 'normal 'haskell-mode-map
    "o" #'vs/--haskell-evil-open-below
    "O" #'vs/--haskell-evil-open-above)

  ;; Elfeed
  (general-def 'normal 'elfeed-search-mode-map
    "S" #'elfeed-search-clear-filter
    "gr" #'elfeed-search-fetch)

  ;; Compilation
  (general-def 'normal 'compilation-mode-map
    "h" #'evil-backward-char)

  (general-def 'insert 'eshell-mode-map
    "C-d" #'eshell-life-is-too-much)

  (general-def 'normal 'dired-mode-map
    "C-j" #'vs/dired-zoxide
    "C-<return>" #'vs/dired-xdg-open
    "?" #'vs/dired-du)

  (general-def 'normal 'ytdious-mode-map
    "j" #'ytdious-next-line
    "k" #'ytdious-previous-line
    "q" #'ytdious-quit
    "s" #'ytdious-search
    "y" #'vs/ytdious-copy-url
    "<return>" #'vs/ytdious-play-mpv)

  (general-def 'insert 'mozc-mode-map
    "C-j" (lambda () (mozc-handle-event 'enter)))
  ;; Selectrum
  (general-def 'selectrum-minibuffer-map
    "C-j" #'selectrum-next-candidate
    "C-k" #'selectrum-previous-candidate
    "C-w" #'evil-delete-backward-word))

(defun vs/--evil-normal-then-ex ()
  "Switch to normal mode and activate ex."
  (interactive)
  (evil-normal-state)
  (evil-ex))

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
