function emacs-add-project --argument dir
    if test -n dir
        set dir (pwd)
    end
    set dir "\"$dir\""
    emacsclient -a "" --eval "(ignore (and (projectile-project-p $dir) (projectile-add-known-project $dir)))"
end
