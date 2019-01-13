#!/bin/bash

ID=$(xdpyinfo | grep -m1 focus | cut -d " " -f4)
PID=$(($(xprop -id $ID | grep -m1 PID | cut -d " " -f3) + 2))
if [ -e "/proc/$PID/cwd" ]
then
	alacritty --working-directory $(readlink /proc/$PID/cwd)
else
	alacritty
fi
