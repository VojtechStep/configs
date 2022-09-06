#!/bin/sh

ID=$(xinput list | rg "SYNA7" | cut -f2 | cut -d"=" -f2)

[ -z "$ID" ] && exit

xinput --map-to-output $ID "eDP1"
