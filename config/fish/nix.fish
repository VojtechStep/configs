if set -q HOME USER
    set -l NIX_LINK $HOME/.nix-profile
    set -l chans "$HOME/.nix-defexpr/channels"
    if not contains $chans $NIX_PATH
        set -xa NIX_PATH $chans
    end
    set -x NIX_PROFILES "/nix/var/nix/profiles/default $HOME/.nix-profile"
    for c in \
        /etc/ssl/certs/ca-certificates.crt \
        /etc/ssl/ca-bundle.pem \
        /etc/ssl/certs/ca-bundle.crt \
        /etc/pki/tls/certs/ca-bundle.crt \
        "$NIX_LINK/etc/ssl/certs/ca-bundle.crt" \
        "$NIX_LINK/etc/ca-bundle.crt"
        if test -e $c
            set -x NIX_SSL_CERT_FILE $c
            break
        end
    end

    set -l mans NIX_LINK/share/man
    if set -q MANPATH and not contains $mans $MANPATH
        set -xp MANPATH $mans
    end

    fish_add_path -P "$NIX_LINK/bin"

    set -l compl "$NIX_LINK/share/fish/vendor_completions.d"
    if not contains $compl $fish_complete_path
        set -p fish_complete_path $compl
    end

    set -l func "$NIX_LINK/share/fish/vendor_functions.d"
    if not contains $func $fish_function_path
        set -p fish_function_path $func
    end
end
