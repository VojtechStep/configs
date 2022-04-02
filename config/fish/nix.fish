if set -q HOME USER
    set -l NIX_LINK $HOME/.nix-profile
    set -x --append NIX_PATH $HOME/.nix-defexpr/channels
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
    if set -q MANPATH
        set -x --prepend MANPATH "$NIX_LINK/share/man"
    end
    set -x --prepend PATH "$NIX_LINK/bin"
    set -x --prepend fish_complete_path "$NIX_LINK/share/fish/vendor_completions.d"
    set -x --prepend fish_function_path "$NIX_LINK/share/fish/vendor_functions.d"
end
