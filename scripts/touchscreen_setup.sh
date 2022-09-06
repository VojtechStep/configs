#!/bin/sh

ID=$(xinput list | rg "SYNA7" | cut -f2 | cut -d"=" -f2)

xinput --map-to-output $ID "eDP1"
