#!/bin/bash

ID=$(xdpyinfo | grep -m1 focus | cut -d " " -f4)
PID=$(($(xprop -id $ID | grep -m1 PID | cut -d " " -f3) + 2))
ALTERN_WD=$(xprop -id $ID | grep -m1 -w WM_NAME | cut -d= -f2 | tr -d "\"" | rev | cut -d" " -f1 | rev)
if [ -e "/proc/$PID/cwd" ]; then
	alacritty --working-directory $(readlink /proc/$PID/cwd)
elif [ -d "$ALTERN_WD" ]; then
	alacritty --working-directory $ALTERN_WD
else
	alacritty
fi
