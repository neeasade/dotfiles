#!/bin/sh

launch() {
    echo setsid nohup "$*" &
    setsid nohup "$*" &
}

launch qutebrowser
launch emacs

# launch $TERMINAL
# colemak
