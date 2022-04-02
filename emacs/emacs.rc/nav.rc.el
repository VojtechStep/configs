;;; nav.rc.el --- Configuration for navigating projects and the Emacs interface  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'rx))

(use-package projectile
  :straight t
  :custom
  (compilation-read-command nil)
  ;; Use default completing-read, which is overriden by selectrum
  (projectile-completion-system 'default)
  (projectile-current-project-on-switch 'keep)
  (projectile-git-command "fd . -H0E .git --type f --color=never")
  (projectile-generic-command "fd . -H0 --type f --color=never")
  :config
  (projectile-register-project-type
   'npm '("package.json")
   :project-file "package.json"
   :configure "yarn install"
   :compile "yarn build"
   :test "yarn test"
   :run "yarn dev")
  (projectile-register-project-type
   'zig '("build.zig")
   :project-file "build.zig"
   :compile "zig build"
   :run "zig build run")
  (projectile-register-project-type
   'weblorg '("publish.el")
   :project-file "publish.el"
   :compile "emacs --script publish.el")
  (cl-pushnew "pom.xml" projectile-project-root-files-bottom-up) ; Have Maven projects index from the top (parent modules)
  (projectile-register-project-type
   'mvn '("pom.xml")
   :compile "mvn compile"
   :test "mvn test"
   :run "mvn exec:java")
  (cl-pushnew "CMakeLists.txt" projectile-project-root-files-bottom-up)
  (projectile-register-project-type
   'cmake '("CMakeLists.txt")
   :configure "cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES && ln -s Debug/compile_commands.json ."
   :compile "cmake --build Release")
  (cl-pushnew "hall.scm" projectile-project-root-files-bottom-up)
  (projectile-register-project-type
   'hall '("hall.scm")
   :project-file "hall.scm"
   :compile "autoreconf -vif && ./configure && make check")
  (projectile-global-mode))

(use-package rg
  :straight t
  :disabled t
  :commands rg-menu
  :config
  (require 'evil-collection)
  (evil-collection-rg-setup))

(use-package selectrum
  :straight t
  :custom
  (enable-recursive-minibuffers t)
  (minibuffer-follows-selected-frame nil)
  :custom-face
  (selectrum-current-candidate ((t (:weight bold
                                            :background "#373b41"
                                            :foreground "#ffffff"))))
  (selectrum-primary-highlight ((t (:foreground "#b5bd68"))))
  (selectrum-secondary-highlight ((t (:foreground "#8abeb7"))))
  :init
  ;; Apparently, this does not *load* selectrum
  (selectrum-mode))

(use-package prescient
  :straight t
  :custom
  (prescient-history-length 1000)
  ;; (prescient-filter-method '(literal initialism fuzzy))
  :config
  (prescient-persist-mode))

(use-package selectrum-prescient
  :straight t
  :demand
  :after selectrum
  :config
  (selectrum-prescient-mode))

(use-package consult
  :straight t)

(use-package consult-selectrum
  :after selectrum
  :demand t)

(use-package vterm
  :straight t
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-shell "/usr/bin/fish")
  :config
  (require 'evil-collection)
  (evil-collection-vterm-setup))

(use-package profiler
  :hook
  (profiler-report-mode . hl-line-mode))

(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . hl-line-mode)
  (dired-mode . display-line-numbers-mode)
  :custom
  (dired-auto-revert-buffer t)
  :init
  (defun vs/dired-zoxide (query)
    "Run zoixde with QUERY and open dired there."
    (interactive (list (read-from-minibuffer "Jump Query: ")))
    (require 'vs-utils.rc)
    (let ((dir (vs/zoxide-query query)))
      (if (file-directory-p dir)
          (dired dir)
        (error "Error jumping: %s" dir))))
  (defun vs/dired-xdg-open ()
    "Open selected file with `xdg-open'."
    (interactive)
    (when-let ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))
  (defun vs/dired-du ()
    "Print total size of marked files."
    (interactive)
    (when-let ((files (dired-get-marked-files))
               (du (executable-find "du")))
      (with-temp-buffer
        (apply #'call-process du nil t nil "-sch" files)
        (re-search-backward
         (rx (group bol
                    (1+ (any hex ?. ?,))
                    (1+ alpha))
             (*? anychar)
             "total"
             eol))
        (message "Marked size: %s" (match-string 1)))))
  (defun vs/--dired-find-file-zoxide-add (&rest _)
    "Function to call after entering a directory with dired.

Should be used as an advice on `dired--find-file'."
    (when (eq major-mode 'dired-mode)
      (vs/zoxide-register default-directory)))
  :config
  (setq dired-listing-switches "-alh") ; Putting it in `:custom' force loads the package
  (advice-add #'dired--find-file :after #'vs/--dired-find-file-zoxide-add)
  (require 'evil-collection)
  (evil-collection-dired-setup))

(provide 'nav.rc)

;;; nav.rc.el ends here
