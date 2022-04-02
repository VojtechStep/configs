#!/bin/sh

if [ -d "$SCREENSHOT_DIR" ]; then
  fd --change-older-than 1day . "$SCREENSHOT_DIR" -X rm
fi
