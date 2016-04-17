#!/usr/bin/env dash

# xrandr script(generated with arandr)
~/.screenlayout/display.sh

# pcmanfm daemon - useful for automatic mounting of drives
pcmanfm -d &

# Xresources
xrdb -load $HOME/.Xresources &

# background
. ~/.bspwm_theme
$BG_COMMAND &

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

# java fixes:
wmname LG3D &
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'  export _JAVA_AWT_WM_NONREPARENTING=1

# check if existing socket exists and killl for bspwm(doesn't play nice with logging out and in)
rm /tmp/$(ls /tmp | grep bspwm)

# set shell that sxhkd uses to execute commands:
export SXHKD_SHELL=dash

# start bspwm and sxhkd
sxhkd -f 100 -c $HOME/.config/sxhkd/sxhkdrc &
exec bspwm -c $HOME/.wm/bspwmrc > .xinit.log 2>&1
