;;; org.rc.el --- Configuration for Org mode         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Vojtech Stepancik

;; Author: Vojtech Stepancik <vojtechstepancik@outlook.com>
;; Keywords: convenience, extensions, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'pcase)
  (require 'use-package))

(use-package org
  :straight (org :host github
                 :repo "emacs-straight/org-mode"
                 :local-repo "org"
                 :depth full)
  :commands (vs/org-agenda vs/org-clock-in vs/org-clock-out vs/org-open-agenda-file)
  :hook
  (org-mode . org-indent-mode)
  (org-mode . visual-line-mode)
  (tex-mode . (lambda ()
                (remove-hook 'after-save-hook #'delete-trailing-whitespace)))
  (org-metareturn . vs/org-meta-return-link-hook)
  :custom
  (org-log-done 'time)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-agenda-window-setup 'other-frame)
  (org-special-ctrl-a/e t)
  (org-directory "~/Org/")
  (org-agenda-files (directory-files-recursively org-directory "\\`[^.].*\\.org\\'"))
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-use-fast-todo-selection 'expert)
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w@)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-todo-keyword-faces
   '(("CANCELLED" . "#fff")
     ("WAITING" . (:foreground "black" :background "white"))))
  (org-use-tag-inheritance nil)
  (org-time-stamp-custom-formats
   '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a [%H:%M]>"))
  (org-clock-x11idle-program-name "xprintidle")
  (org-deadline-warning-days 7)
  (org-clock-persist 'history)
  (org-clock-idle-time 10)
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-edit-src-content-indentation 0)
  (org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
  (org-emphasis-alist
   '(("$" tex-math)
     ("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+" (:strike-through t))))
  (org-confirm-babel-evaluate nil)
  (org-latex-listings 'minted)
  ;; (org-latex-packages-alist '(("newfloat" "minted")))
  (org-latex-pdf-process
   '("latexmk -pdflatex=\"pdflatex -shell-escape -interaction nonstopmode\" -xelatex=\"xelatex -shell-escape -interaction nonstopmode\" -%latex -f -outdir=%o %f"))
  (org-structure-template-alist
   '(("c" . "comment")
     ("d" . "definition")
     ("e" . "example")
     ("E" . "export")
     ("l" . "export latex")
     ("p" . "proof")
     ("s" . "src")))
  :config
  (defun vs/org-agenda (&optional arg)
    (interactive)
    (require 'org-agenda)
    (let ((org-agenda-tag-filter-preset '("-drill")))
      (org-agenda arg)))
  (defun vs/org-meta-return-link-hook ()
    (when-let* ((ctx (org-element-context))
                ((eq (org-element-type ctx) 'link)))
      (org-link-open ctx)
      t))
  (defun vs/org-clock-in ()
    "Clock in the current entry.
If not in an org or agenda buffer, clock in the last task."
    (interactive)
    (pcase major-mode
      ('org-agenda-mode (org-agenda-clock-in))
      ('org-mode (org-clock-in))
      (_ (org-clock-in '(4)))))
  (defun vs/org-clock-out ()
    "Clock out of the current entry."
    (interactive)
    (pcase major-mode
      ('org-agenda-mode (org-agenda-clock-out))
      (_ (org-clock-out))))
  (defun vs/org-open-agenda-file ()
    (interactive)
    (find-file (expand-file-name
                (completing-read "Open agenda file: "
                                 (org-agenda-files)
                                 nil
                                 t))))
  (org-clock-persistence-insinuate))

(use-package org-src
  :after org
  :custom
  (org-src-window-setup 'other-frame)
  :hook
  (org-src-mode . (lambda ()
                    (when (fboundp 'flycheck-disable-checker)
                      (flycheck-disable-checker 'emacs-lisp-checkdoc))))
  :config
  (cl-pushnew '("nix-shell" . sh)
              org-src-lang-modes))

(use-package htmlize
  :straight t)

(use-package org-capture
  :after org
  :custom
  (org-capture-templates nil)
  :hook
  (org-capture-mode . evil-insert-state)
  :config
  (cl-pushnew `("n" "Note to clocking" item (clock)
                ,(concat "- " (alist-get 'note org-log-note-headings) " \\\\
  %?")
                :clock-keep t)
              org-capture-templates)
  (let ((todofile "TODO.org")
        (taskfile "Tasks.org")
        (learnfile "Learn.org")
        (vocabfile "Vocabulary.org")
        (journalfile "Diary.org"))
    (dolist (tpl `(("e" "Emacs todo" entry
                    (file+olp ,todofile "Todos" "Emacs")
                    "** TODO %?")
                   ("s" "System todo" entry
                    (file+olp ,todofile"Todos" "System")
                    "** TODO %?")
                   ("l" "Learn stuff" entry
                    (file ,learnfile)
                    "* LEARN %?"
                    :empty-lines 1)
                   ("v" "Vocabulary" entry
                    (file+olp+datetree ,vocabfile)
                    "* %^{Vocab} :drill:
Reading: [%^{Reading}]
Meaning: [%^{Meaning}]
Kanji: [%^{Kanji}]")
                   ("t" "Task" entry
                    (file+olp+datetree ,taskfile)
                    "* TODO %?
SCHEDULED: %t")
                   ("d" "Diary" plain
                    (file+olp+datetree ,journalfile)
                    "%?")))
      (cl-pushnew tpl org-capture-templates :test #'equal))))

(declare-function evil-collection-calendar-setup 'evil-collection)
(use-package calendar
  :config
  (require 'evil-collection)
  (evil-collection-calendar-setup))

(use-package org-attach
  :demand t
  :after org
  :custom
  (org-attach-use-inheritance t))

(use-package org-ref
  :disabled t
  :straight t
  :demand t
  :after org
  :custom
  (reftex-default-bibliography
   (list (expand-file-name "bibliography/references.bib"
                           org-directory)))
  (org-ref-default-bibliography
   (list (expand-file-name "bibliography/references.bib"
                           org-directory)))
  (org-ref-bibliography-notes
   (expand-file-name "bibliography/notes.org"
                     org-directory))
  (org-ref-pdf-directory
   (expand-file-name "~/Books/Tech")))

(use-package org-projectile
  :straight t
  :hook
  ((org-mode org-agenda-mode org-capture-mode)
   . (lambda ()
       (require 'org-projectile)))
  (projectile-after-switch-project . vs/org-projectile-refresh-agendas)
  :custom
  (org-projectile-per-project-filepath "TODO.org")
  :commands vs/org-capture
  :init
  (defun vs/org-capture ()
    "Call `org-capture', but require `org-projectile' first."
    (interactive)
    (require 'org-projectile)
    (call-interactively #'org-capture))
  :config
  (defun vs/org-projectile-refresh-agendas ()
    (interactive)
    (require 'seq)
    (setq org-agenda-files (seq-filter #'file-readable-p
                                       (delete-dups
                                        (append org-agenda-files
                                                (org-projectile-todo-files))))))
  (org-projectile-per-project)
  (vs/org-projectile-refresh-agendas)
  (cl-pushnew (org-projectile-project-todo-entry) org-capture-templates))

(use-package evil-org
  :straight t
  :after (evil org)
  :hook
  (org-mode . evil-org-mode)
  (org-agenda-mode . (lambda ()
                       (require 'evil-org-agenda)
                       (evil-org-agenda-set-keys)))
  :custom
  (evil-org-key-theme '(navigation insert return additional todo))
  (evil-org-use-additional-insert t)
  :config
  (evil-org-set-key-theme))

(use-package org-drill
  :straight (org-drill :type git
                       :flavor melpa
                       :host gitlab
                       :repo "phillord/org-drill"
                       :protocol https)
  :commands (org-drill vs/org-drill-cram)
  :hook
  (org-drill-response-mode . (lambda ()
                               (text-scale-set 4)))
  :custom
  (org-drill-learn-fraction 0.01)
  (org-drill-save-buffers-after-drill-sessions-p nil)
  (org-drill-scope 'agenda)
  :init
  (defun vs/org-drill-cram ()
    "Cram all items in a subtree."
    (interactive)
    (let ((org-drill-scope 'tree)
          (org-drill-cram-hours 0)
          org-drill-maximum-items-per-session)
      (org-drill-cram))))

(use-package ox-hugo
  :straight t
  :demand
  :after ox)

(provide 'org.rc)
;;; org.rc.el ends here
