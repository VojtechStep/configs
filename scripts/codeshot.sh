#!/bin/sh

if [ "$#" -ne 5 ]; then
  echo "Usage: codeshot.sh WINID XOFFSET YOFFSET WIDTH HEIGHT"
  exit 1
fi

WINID="$1"
XOFFSET="$2"
YOFFSET="$3"
WIDTH="$4"
HEIGHT="$5"
FILENAME="$(screenshot.sh filename Code)"

import -window $WINID -crop "${WIDTH}x${HEIGHT}+${XOFFSET}+${YOFFSET}" "$FILENAME"
xclip -sel clip -t image/png < "$FILENAME"

echo $FILENAME
