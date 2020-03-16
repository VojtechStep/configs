#!/bin/sh

if [ -z "$*" ] || [ "$*" = "-h" ] || [ "$*" = "--help" ]; then
    echo "Usage:"
    echo "  here.sh COMMAND [ARGS]"
    echo ""
    echo "COMMAND		Command to be run"
    echo "ARGS		Arguments to be passed to the command"
    echo ""
    echo "The utility accepts the same arguments as xargs."
    echo ""
    echo "EXAMPLE"
    echo "  here.sh -I{} emacs --chdir \"{}\""
    exit 1
fi

# Get active window and corresponding process
parent=$(xprop -root _NET_ACTIVE_WINDOW \
               | rg -e '# (\w+)' -or '$1' \
               | xargs xprop _NET_WM_PID -id \
               | rg -e ' = (\d+)' -or '$1')

# Get children processes
#
# If the active window is a terminal emulator, the child will
# be the shell process
# 
# If the active window is an emacs instance, there is no child
# 
# If the active window is a chromium instance, there will be multiple
# children
children=$(ps hopid --ppid "$parent" | tr -d ' ')

# Check if the process list is empty or more than one
# (count line ends, short circuiting with head after two matches)
if [ -z "$children" ]; then
    # Zero children - use the cwd of the process
    dir=$(readlink -e /proc/$parent/cwd)
elif [ $(echo "$children" | rg '$' | head -2 | wc -l) -ne "1" ]; then
    # Multiple children - use home
    dir=$HOME
else
    # One child - use it's cwd
    dir=$(readlink -e /proc/$children/cwd)
fi

echo $dir | xargs $@
