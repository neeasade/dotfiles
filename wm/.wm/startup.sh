#!/bin/sh

launch() {
    echo setsid nohup "$*" &
    setsid nohup "$*" &
}

launch qutebrowser
launch 'emacs-28.0.9'

# launch $TERMINAL
# colemak
