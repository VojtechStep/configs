#!/usr/bin/env sh

echo $(fzf <$1) > $1
