#!/bin/sh

get_stylus() {
  xsetwacom list devices | rg "stylus \s*id: (\d+)" -or\$1
}

setup_ultrawide() {
  STYLUS=$(get_stylus)
  xsetwacom set $STYLUS MapToOutput 1945x1200+0+0
}

setup_single() {
  STYLUS=$(get_stylus)
  xsetwacom set $STYLUS MapToOutput 1920x1200+3840+0
}

# case $1 in
#   ultrawide) setup_ultrawide;;
#   single) setup_single;;
# esac
