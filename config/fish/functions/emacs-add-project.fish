function emacs-add-project --argument dir
  if test -n dir
    set dir (pwd)
  end
  emacsclient -a "" --eval "(ignore (and (require 'vs-utils.rc) (vs/add-project-maybe \"$dir\")))"
end
