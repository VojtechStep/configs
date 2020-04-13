#!/bin/sh

echo $(fzf <$1) > $1
