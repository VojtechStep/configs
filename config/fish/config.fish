# Environment variables

if test -f $__fish_config_dir/environment.fish
    source $__fish_config_dir/environment.fish
end

# Interactive candy
if status --is-interactive

    type direnv &>/dev/null && direnv hook fish | source

    if type zoxide &>/dev/null
        zoxide init fish --no-aliases | source
        function z
            __zoxide_z $argv
        end
        function zi
            __zoxide_zi $argv
        end
        function za
            __zoxide_za $argv
        end
        function zr
            __zoxide_zr $argv
        end
    end

    # Taken from https://github.com/akermu/emacs-libvterm#shell-side-configuration
    function vterm-printf
        if [ -n "$TMUX" ]
            # tell tmux to pass the escape sequences through
            # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
            printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
        else if string match -q -- "screen*" "$TERM"
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$argv"
        else
            printf "\e]%s\e\\" "$argv"
        end
    end
    function vterm-prompt-end
        vterm-printf "51;A"(whoami)"@"(uname -n)":"(pwd)
    end
    functions --copy fish_prompt vterm-old-prompt &>/dev/null
    function fish_prompt
        set -l old_prompt (string join "\n" (vterm-old-prompt))
        if set -q IN_NIX_SHELL
            set_color cyan
            printf "[N] "
        end
        printf "%b" $old_prompt
        vterm-prompt-end
    end

    abbr -a v $VISUAL
    abbr -a e emacsclient -c

    abbr -a rm rm -I

    abbr -a ls exa -l
    abbr -a la exa -la

    abbr -a shutdown systemctl poweroff

    abbr -a gis git status
    abbr -a gif git fetch
    abbr -a gip git pull
    abbr -a gipl git plog
    abbr -a gipo git plog HEAD..origin/master
    abbr -a gdo git diff HEAD origin/master

    abbr -a vf nvim ~/.config/fish/config.fish
    abbr -a vv nvim ~/.config/nvim/init.vim

    abbr -a serve miniserve --index index.html

    abbr -a sc systemctl
    abbr -a scu systemctl --user
    abbr -a ssc sudo systemctl
    abbr -a bt bluetoothctl
end

# Login shell
if status --is-login
    if test -z "$DISPLAY" -a $XDG_VTNR = 1
        exec startx "$XINITRC" -- -ardelay 300 -arinterval 30 &>"$XDG_DATA_HOME/xorg/Xorg.log"
    end
end
