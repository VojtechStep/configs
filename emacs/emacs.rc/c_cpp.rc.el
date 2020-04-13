;;; c_cpp.rc.el --- Configuration for C/C++ files

;;; Commentary:
;; 

;;; Code:

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

(defun vs/--cmake-ccls-setup-numbered-folder (base name &optional num)
  "Make a folder called NAME in BASE with the suffix NUM.
If the folder already exists, increment NUM and try again."
  (let* ((new-name (concat name (if num (number-to-string num) "")))
		 (new-path (expand-file-name new-name base)))
	(if (file-exists-p new-path)
		(cmake-ccls-setup--numbered-folder base name (if num (+1 num) 1))
	  new-path)))

(defun vs/cmake-ccls-setup ()
  "Setup compilation of CMake projects."
  (interactive)
  (let* ((projroot (projectile-project-root))
         (cmakefile (expand-file-name "CMakeLists.txt" projroot))
         (compile-commands (expand-file-name "compile_commands.json" projroot))
         (ignore-file (expand-file-name ".ignore" projroot)))
    (when (and							; Only prompt if: There is a CMakeLists.txt AND it's readable AND there is no compile_commands.json AND the use agrees
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
