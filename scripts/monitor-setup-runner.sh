#!/bin/sh

layout=$(echo "auto\ndouble-invert\ndouble-normal\ndouble-lab\nduplicate\nsingle" | rofi -dmenu)
monitor_setup.sh $layout
