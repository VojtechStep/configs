function load-env -a script
  source $script $script
end

load-env ~/.envs.el

set -U fish_user_paths
abbr -a :q exit

abbr -a v $VISUAL
abbr -a e emacsclient -c
abbr -a em emacs
abbr -a rem systemctl --user restart emacs
abbr -a eaf 'emacsclient --eval "(projectile-add-known-project \""(pwd)"\")"'

abbr -a qmk 'docker run -it --rm -v (pwd)":/qmk_firmware" qmkfm/base_container make -C /qmk_firmware/ ergodox_ez:vojtechstep'


abbr -a ls exa -l
abbr -a la exa -la

abbr -a shitdown systemctl poweroff
abbr -a shutdown systemctl poweroff

abbr -a maek make

abbr -a a archey4

abbr dr docker run -it --rm

abbr gis git status
abbr gif git fetch
abbr gip git pull
abbr gipl git plog
abbr gipo git plog origin/master
abbr gdo git diff HEAD origin/master

abbr vf nvim ~/.config/fish/config.fish
abbr vv nvim ~/.config/nvim/init.vim

if status --is-login
  if test -z "$DISPLAY" -a $XDG_VTNR = 1
    exec startx -- -ardelay 300 -arinterval 30
  end
end

if test -f ~/.config/fish/autojump.fish
  source ~/.config/fish/autojump.fish
end

type nvm >/dev/null ^/dev/null; and nvm use 10
