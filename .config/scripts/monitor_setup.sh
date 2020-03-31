#! /usr/bin/sh

# xrandr --output DP-1 --auto --right-of eDP-1 --scale 2x2
# xrandr --output DVI-I-1-1 --auto --left-of eDP-1 --output DVI-I-2-2 --auto --right-of eDP1 && ~/.fehbg
xrandr --output DP1 --auto --primary --scale 2x2 --output eDP1 --pos 7680x0 && ~/.config/fehbg
