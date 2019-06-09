set -xg GDK_SCALE 2
set -xg PATH $HOME/.fzf/bin $PATH
set -xg PATH $HOME/.yarn/bin $PATH
set -xg PATH $HOME/.cargo/bin $PATH
set -xg PATH $HOME/.local/bin $PATH
set -xg PATH $HOME/.pub-cache/bin $PATH
set -xg PATH $HOME/.dotnet/tools $PATH
set -xg PATH /opt/android-sdk/tools/bin $PATH
set -xg PATH /opt/android-sdk/platform-tools $PATH
set -xg MCU_IDE_DIR /usr/local/mcuxpresso-ide
set -xg ANDROID_HOME /opt/android-sdk
set -xg DOTNET_ROOT /opt/dotnet
set -xg VISUAL nvim
set -xg EDITOR nvim
set -xg BROWSER firefox

set -U fish_user_paths

abbr -a v nvim
abbr -a vim nvim

abbr -a ls exa -l
abbr -a la exa -la

abbr -a ghub git clone https://github.com/

abbr -a shitdown shutdown now
abbr -a shutdown shutdown now

abbr -a acmc g++ -lm -lcrypt -O2 -std=c++11 -pipe -DONLINE_JUDGE -o main ./main.cpp

abbr -a a archey3

if status --is-login
	if test -z "$DISPLAY" -a $XDG_VTNR = 1
		exec startx
	end
end

if test -f $HOME/.autojump/share/autojump/autojump.fish
	source $HOME/.autojump/share/autojump/autojump.fish
end

type nvm >/dev/null ^/dev/null; and nvm use 12
