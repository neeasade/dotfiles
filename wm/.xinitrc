#!/bin/sh

. $HOME/.sh.d/environment

if ! [ "$(hostname)" = erasmus ]; then
    xbright
fi

wmname LG3D
xsetroot -cursor_name left_ptr
export xstart=$(date +%s)

# launched by display manager bspwm desktop entry.
# sxhkd -c $HOME/.config/sxhkd/sxhkdrc &

if ${RUNWM:-true}; then
    exec $HOME/.wm/wm
fi
