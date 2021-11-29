#!/bin/sh

launch() {
    echo setsid nohup "$*" &
    setsid nohup "$*" &
}

launch qutebrowser
launch emacs-27.2
# launch $TERMINAL
# colemak
