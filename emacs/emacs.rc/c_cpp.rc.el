;;; c_cpp.rc.el --- Configuration for C/C++ files  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

;; (use-package ccls
;;   :defer t
;;   :custom
;;   (ccls-initialization-options '(:cache (:directory "/tmp/ccls")))
;;   (ccls-sem-highlight-method 'overlay)
;;   :hook
;;   (ccls-tree-mode . (lambda ()
;;                       (setq-local display-line-numbers nil))))

;; (lsp-register-client (make-lsp-client
;;                       :server-id 'clangd
;;                       :new-connection (lsp-stdio-connection "clangd")
;;                       :major-modes '(c++-mode c-mode)
;;                       :initialization-options "--clang-tidy"))

(defvar clang-format '(clang-format "clang-format" "--assume-filename"
                                    file))

(use-package cc-mode
  :hook
  (cc-mode . apheleia-mode)
  (java-mode . apheleia-mode)
  :config
  (when (require 'apheleia nil t)
    (defvar apheleia-formatters)
    (defvar apheleia-mode-alist)
    (cl-pushnew clang-format apheleia-formatters)
    (cl-pushnew '(c-mode . clang-format) apheleia-mode-alist)
    (cl-pushnew '(c++-mode . clang-format) apheleia-mode-alist)
    (cl-pushnew '(java-mode . clang-format) apheleia-mode-alist)))

(use-package cmake-mode
  :straight t)

(defun vs/--cmake-ccls-setup-numbered-folder (base name &optional num)
  "Make a folder called NAME in BASE with the suffix NUM.
If the folder already exists, increment NUM and try again."
  (let* ((new-name (concat name (if num (number-to-string num) "")))
		 (new-path (expand-file-name new-name base)))
	(if (file-exists-p new-path)
		(vs/--cmake-ccls-setup-numbered-folder base name (if num (1+ num) 1))
	  new-path)))

(defun vs/cmake-ccls-setup ()
  "Setup compilation of CMake projects."
  (interactive)
  (require 'projectile)
  (require 'f)
  (let* ((projroot (projectile-project-root))
         (cmakefile (expand-file-name "CMakeLists.txt" projroot))
         (compile-commands (expand-file-name "compile_commands.json" projroot))
         (ignore-file (expand-file-name ".ignore" projroot)))
    (when (and							; Only prompt if: There is a CMakeLists.txt AND it's readable AND there is no compile_commands.json AND the use agrees
           (require 'f nil t)
           (file-readable-p cmakefile)
           (not (file-exists-p compile-commands))
           (y-or-n-p "Compile_commands.json missing.  Generate? "))
      (message "Generating")
      (let ((debug-folder (vs/--cmake-ccls-setup-numbered-folder projroot "Debug")))
        (shell-command (format "cmake -H. -B%s -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES" debug-folder))
        (f-symlink (expand-file-name "compile_commands.json" debug-folder) projroot))
      (message "Not generating"))
    (when (and
           (not (file-exists-p ignore-file))
           (y-or-n-p "Ignore file does not exist.  Generate? "))
      (with-temp-file ignore-file
        (insert "Release")
        (newline)
        (insert "Debug")))))

(provide 'c_cpp.rc)

;;; c_cpp.rc.el ends here
