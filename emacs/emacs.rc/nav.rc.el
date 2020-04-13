;;; nav.rc.el --- Configuration for navigating projects and the Emacs interface

;;; Commentary:
;; 

;;; Code:

(use-package projectile
  :straight t
  :custom
  (compilation-read-command nil)
  :config
  (cl-pushnew "package.json" projectile-project-root-files) ; Node projects
  (projectile-register-project-type
   'npm
   '("package.json")
   :configure "yarn install"
   :compile "yarn build"
   :test "yarn test"
   :run "yarn dev")
  (cl-pushnew "pom.xml" projectile-project-root-files-bottom-up) ; Have Maven projects index from the top (parent modules)
  (projectile-register-project-type
   'mvn
   '("pom.xml")
   :compile "mvn compile"
   :run "mvn exec:java")
  (cl-pushnew "CMakeLists.txt" projectile-project-root-files-bottom-up)
  (projectile-register-project-type
   'cmake '("CMakeLists.txt")
   :configure "cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES && ln -s Debug/compile_commands.json ."
   :compile "cmake --build Release")
  (projectile-global-mode))

;; Fuzzy filtering
(use-package helm
  :straight t
  :custom
  (helm-window-prefer-horizontal-split 'decide)
  :config
  (require 'helm-config)
  (helm-autoresize-mode t)
  (helm-mode))

(use-package helm-projectile
  :straight t
  :commands (helm-projectile-switch-project
             helm-projectile-find-file)
  :custom
  (projectile-completion-system 'helm)
  :config
  (helm-projectile-on))

(use-package helm-ag
  :straight t
  :after helm
  :custom
  (helm-ag-base-command "rg --smart-case --no-heading --color=never --hidden --line-number"))

(use-package vterm
  :straight t
  :custom
  (vterm-kill-buffer-on-exit t))

(provide 'nav.rc)

;;; nav.rc.el ends here
