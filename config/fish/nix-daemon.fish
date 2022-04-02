if set -q __ETC_PROFILE_NIX_SOURCED
  return
end
set -x __ETC_PROFILE_NIX_SOURCED 1

set -x NIX_PROFILES "/nix/var/nix/profiles/default $HOME/.nix-profile"

if not set -q NIX_SSL_CERT_FILE
  for c in \
    /etc/ssl/certs/ca-certificates.crt \
    /etc/ssl/ca-bundle.pem \
    /etc/ssl/certs/ca-bundle.crt \
    /etc/pki/tls/certs/ca-bundle.crt
    if test -e $c
      set -x NIX_SSL_CERT_FILE $c
      break
    end
  end
  if not set -q NIX_SSL_CERT_FILE
    for p in $NIX_PROFILES
      if test -e $p/etc/ssl/certs/ca-bundle.crt
        set -x NIX_SSL_CERT_FILE $p/etc/ssl/certs/ca-bundle.crt
      end
    end
  end
end

set -x NIX_PATH "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs" "/nix/var/nix/profiles/per-user/root/channels"
set -x --prepend PATH "$HOME/.nix-profile/bin" "/nix/var/nix/profiles/default/bin"
