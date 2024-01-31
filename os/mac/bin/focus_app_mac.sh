#!/bin/sh

process=$*

# emacs breaks if you do this -- unclear
# if ! pgrep "$process"; then
#     exit 1
# fi

osascript -e "tell application \"System Events\" to tell process \"${process}\"" \
	  -e 'set frontmost to true' \
	  -e 'if windows is not {} then perform action "AXRaise" of item 1 of windows' \
	  -e 'end tell'
