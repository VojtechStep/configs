#!/bin/sh

ID=$(xinput list | grep Touchpad | cut -f2 | cut -d"=" -f2)

xinput --set-prop $ID 'libinput Accel Speed' 0.5
xinput --set-prop $ID 'libinput Tapping Enabled' 1
xinput --set-prop $ID 'libinput Natural Scrolling Enabled' 1
xinput --set-prop $ID 'libinput Middle Emulation Enabled' 1

