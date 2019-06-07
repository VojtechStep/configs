[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

setxkbmap -option caps:ctrl_modifier
unclutter &

xrandr --dpi 315
~/.config/scripts/monitor_setup.sh &
~/.config/scripts/display_adj.sh &
~/.config/scripts/touchpad_setup.sh &
~/.config/scripts/dwm_status.sh &
setxkbmap -layout us,cz

~/.fehbg &
# exec compton &
# exec i3

termite &
firefox &
xbindkeys

exec dwm
