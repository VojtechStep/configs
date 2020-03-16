#!/usr/bin/env sh

if [ "$(pgrep transmission-daemon)" = "" ]; then
	transmission-daemon
fi

transmission-remote --add "$1"
~/.config/scripts/notify.sh "Torrent added to download queue"