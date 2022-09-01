# Everything in this file should be idempotent, as it is sourced twice -
# once for login, and once for regular sessions.

# Keep in mind that exported variables (set with -x) are exported
# to the lower fish sessions as well, and they go through dash.
# That causes problems with path variables set at login,
# then are serialized through dash when exec-ing the window manager,
# and then modified again from fish, without telling it
# that they are path variables.

# As a rule of thumb, don't export fish internal variables,
# specify --path everywhere it makes sense, use fish_add_path
# for modifications of PATH, and check if an element exists
# in a list variable with the contains builtin, before making
# any --append or --prepend operations.

# Remember to copy the variable names in Emacs
set -xg CXX /usr/bin/clang++
set -xg CC /usr/bin/clang
set -xg WINIT_X11_SCALE_FACTOR 1
set -xg --path XDG_CONFIG_HOME ~/.config
set -xg --path XDG_CACHE_HOME ~/.cache
set -xg --path XDG_DATA_HOME ~/.local/share
set -xg XAUTHORITY $XDG_RUNTIME_DIR/Xauthority
set -xg XMODIFIERS @im=uim
set -xg XINITRC $XDG_CONFIG_HOME/X11/xinitrc
set -xg GNUPGHOME $XDG_DATA_HOME/gnupg
set -xg DOTFILES_HOME ~/Code/VojtechStep/dotfiles
set -xg CARGO_HOME $XDG_CACHE_HOME/cargo
set -xg NUGET_PACKAGES $XDG_CACHE_HOME/nuget
set -xg AZURE_CONFIG_DIR $XDG_DATA_HOME/azure
set -xg RUSTUP_HOME $XDG_DATA_HOME/rustup
set -xg STACK_ROOT $XDG_DATA_HOME/stack
set -xg DOCKER_CONFIG $XDG_CONFIG_HOME/docker
set -xg CABAL_CONFIG $XDG_CONFIG_HOME/cabal
set -xg CABAL_DIR $XDG_CACHE_HOME/cabal
# set -xg ASPELL_CONF "per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; personal $XDG_CONFIG_HOME/aspell/en.pws; repl $XDG_CONFIG_HOME/aspell/en.prepl; dict-dir $HOME/.nix-profile/lib/aspell"
set -xg ASPELL_CONF "per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; home-dir $XDG_DATA_HOME/aspell; dict-dir $HOME/.nix-profile/lib/aspell"
fish_add_path -P $XDG_CACHE_HOME/cargo/bin ~/.local/bin ~/.local/bin/scripts
set -xg TERMINFO $XDG_CONFIG_HOME/terminfo
set -xg SCREENSHOT_DIR ~/Pictures/Screenshots
set -xg PYTHONSTARTUP $XDG_CONFIG_HOME/python/startup.py
set -xg NPM_CONFIG_USERCONFIG $XDG_CONFIG_HOME/npm/npmrc
set -xg VISUAL nvim
set -xg EDITOR $VISUAL
set -xg BROWSER brave
set -xg FZF_DEFAULT_COMMAND fd --type f --color=always
set -xg FZF_DEFAULT_OPTS --ansi
set -xg LESSHISTFILE -
set -xg LESS --raw-control-chars
set -xg DOTNET_CLI_TELEMENTRY_OPTOUT 1
set -xg DIRENV_LOG_FORMAT
set -xg QT_AUTO_SCREEN_SCALE_FACTOR 1
set -xg MANPAGER "sh -c 'col -bx | bat -l man -p --paging always'"

# For bash
set -xg HISTFILE $XDG_DATA_HOME/bash/history

if test -f $__fish_config_dir/nix.fish
    source $__fish_config_dir/nix.fish
end
