function load-env -a script
  source $script $script
end

load-env ~/.envs.el

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
