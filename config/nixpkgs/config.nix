{
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
      repoOverrides = {
        vojtechstep = import ~/Code/VojtechStep/vspkgs { inherit pkgs; };
      };
    };
  };
}
