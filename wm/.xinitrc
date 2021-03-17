#!/bin/sh

. $HOME/.sh.d/environment

if $LINUX; then
    xrdb -load $HOME/.Xresources
    wmname LG3D
    xsetroot -cursor_name left_ptr
    theme refresh bg
    xbright
fi

export xstart=$(date +%s)

# launched by display manager bspwm desktop entry.
# sxhkd -c $HOME/.config/sxhkd/sxhkdrc &

exec $HOME/.wm/wm
