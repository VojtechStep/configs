[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

setxkbmap -option caps:ctrl_modifier
unclutter &

xrandr --dpi 315
xinput set-prop 11 'libinput Accel Speed' 0.5
xinput set-prop 11 'libinput Tapping Enabled' 1
xinput set-prop 11 'libinput Natural Scrolling Enabled' 1
setxkbmap -layout us,cz

~/.fehbg &
# exec compton &
exec i3
