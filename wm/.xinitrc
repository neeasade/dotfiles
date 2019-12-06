#!/usr/bin/env bash

. $HOME/.sh.d/environment

xrdb -load $HOME/.Xresources &
compton &
unclutter -idle 1 &
pcmanfm -d &
mpd $HOME/.config/mpd/mpd.conf &
dunst &
wmname LG3D &
xsetroot -cursor_name left_ptr &
nm-applet &
(. $HOME/.wm_theme; $BG_COMMAND) &

export xstart=`date +%s`

# launched by display manager bspwm desktop entry.
# sxhkd -c $HOME/.config/sxhkd/sxhkdrc &

exec $HOME/.wm/wm
