#!/usr/bin/env bash

. $HOME/.profile

# xrandr script(generated with arandr)
~/.screenlayout/display.sh

# pcmanfm daemon - useful for automatic mounting of drives
pcmanfm -d &

# Xresources
xrdb -load $HOME/.Xresources &

# background (subshell so x session doesn't get initial theme values.)
(
	. ~/.bspwm_theme
	$BG_COMMAND
) &

# compositing
compton &

# mpd and mpdcron(for notifications and lastfm scrobbling)
mpd ~/.config/mpd/mpd.conf &
MPDCRON_DIR="$HOME/.config/mpd"
[ -z $(pgrep -xU $UID mpdcron) ] && mpdcron &

# make the default x cursor not an x :)
xsetroot -cursor_name left_ptr &

# dunst
dunst &

# cursor dissapear nicety.
unclutter &

# java fixes:
wmname LG3D &
alias java='java -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
export _JAVA_AWT_WM_NONREPARENTING=1

# set shell that sxhkd uses to execute commands:
export SXHKD_SHELL=dash

export xstart=`date +%s`

# launched by display manager bspwm desktop entry.
# sxhkd -c $HOME/.config/sxhkd/sxhkdrc &

exec $HOME/.wm/wm
