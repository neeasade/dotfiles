#!/bin/sh

launch() {
    echo setsid nohup "$*" &
    setsid nohup "$*" &
}

launch qutebrowser
launch emacs-27.1
# launch $TERMINAL
# colemak
