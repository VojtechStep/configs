[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

unclutter &

xrandr --dpi 315
~/.config/scripts/monitor_setup.sh &
~/.config/scripts/display_adj.sh = &
~/.config/scripts/touchpad_setup.sh &
~/.config/scripts/dwm_status.sh &
setxkbmap -option caps:ctrl_modifier -layout us,us,cz -variant ,colemak,qwerty

~/.fehbg &

termite &
firefox &
xbindkeys

exec dwm
