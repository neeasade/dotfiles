#!/bin/sh

launch() {
    echo setsid nohup "$*" &
    setsid nohup "$*" &
}

launch emacs-26.3
launch $TERMINAL
colemak
