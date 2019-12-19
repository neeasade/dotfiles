#!/usr/bin/env bash

. $HOME/.sh.d/environment

xrdb -load $HOME/.Xresources
wmname LG3D
xsetroot -cursor_name left_ptr
$(theme getval BG_COMMAND)

export xstart=$(date +%s)

# launched by display manager bspwm desktop entry.
# sxhkd -c $HOME/.config/sxhkd/sxhkdrc &

exec $HOME/.wm/wm
