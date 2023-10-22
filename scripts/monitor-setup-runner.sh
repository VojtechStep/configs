#!/bin/sh

layout=$(monitor_setup.sh --list-modes | rofi -dmenu)
monitor_setup.sh $layout
