set -xg GDK_SCALE 2
set -xg GOPATH ~/Projects/go
set -xg PATH $HOME/.yarn/bin $PATH
set -xg PATH $HOME/.cargo/bin $PATH
set -xg PATH $HOME/Projects/go/bin $PATH
set -xg PATH $HOME/.local/bin $PATH
set -xg VISUAL nvim
set -xg EDITOR nvim
set -xg BROWSER qutebrowser

set -U fish_user_paths

abbr -a v nvim
abbr -a vim nvim

abbr -a ls exa -l
abbr -a la exa -la

abbr -a todo todolist

abbr -a ghub git clone https://github.com/

abbr -a shutdown shutdown now

abbr -a acmc g++ -lm -lcrypt -O2 -std=c++11 -pipe -DONLINE_JUDGE -o main ./main.cpp

if status --is-login
	if test -z "$DISPLAY" -a $XDG_VTNR = 1
		exec startx
	end
end

if test -f $HOME/.autojump/share/autojump/autojump.fish
	source /home/adalbert/.autojump/share/autojump/autojump.fish
end

type nvm >/dev/null ^/dev/null; and nvm use 10
