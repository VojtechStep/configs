:; cat $argv[1] | sed -E -n -e 's/\(init\/env //p' | sed -E -e 's/\(getenv "(\w+)"\)\s*/\$\1/' -e 's/\)$//' -e 's/([^:]+):append (.*)/-a \1\2/' -e 's/^/set -xg /' | source
:; exit 0
:;'

;;; Code:

(eval-when-compile
  (require (quote cl-lib)))
(require (quote subr-x))

(defmacro init/env (namearg &rest bodyarg)
  "Set environment variable NAMEARG to BODYARG.
If the keyword :append is specified, append to the variable instead."
  `(let* ((name ,(symbol-name namearg))
          (fullbody (mapcar (quote eval) (quote ,bodyarg)))
          (appending (eq (car fullbody) :append))
          (bodies (if appending (cdr fullbody) fullbody))
          (joint (apply (quote concat) bodies))
          (newval (if appending
                      (string-join (list joint (getenv name)) ":")
                    joint)))
     (setenv name newval)))

(init/env GDK_SCALE "2")
(init/env WINIT_X11_SCALE_FACTOR "1.9")
(init/env PATH :append (getenv "HOME") "/.yarn/bin")
(init/env PATH :append (getenv "HOME") "/.cargo/bin")
(init/env PATH :append (getenv "HOME") "/.local/bin")
(init/env PATH :append (getenv "HOME") "/.dotnet/tools")
(init/env PATH :append (getenv "HOME") "/.config/scripts")
(init/env PATH :append (getenv "HOME") "/.nimble/bin")
(init/env PATH :append (getenv "HOME") "/.emacs.doom.d/bin")
(init/env NVM_DIR "/usr/share/nvm")
(init/env VISUAL "nvim")
(init/env EDITOR (getenv "VISUAL"))
(init/env BROWSER "chromium")
(init/env FZF_DEFAULT_COMMAND "fd --type f")
(init/env FZF_DEFAULT_OPTS "--ansi")
(init/env LESSHISTFILE "-")
(init/env DOTNET_CLI_TELEMETRY_OPTOUT "1")
(init/env PIZZA "hello")

(dolist (path (split-string (getenv "PATH") ":"))
  (cl-pushnew path exec-path))

:;'
