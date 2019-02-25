#!/usr/bin/env bash


. $HOME/.sh.d/environment

# xrandr script(generated with arandr)
$HOME/.screenlayout/display.sh

xrdb -load $HOME/.Xresources &
# xmodmap $HOME/.Xmodmap &
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
