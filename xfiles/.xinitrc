#!/usr/bin/env bash

. $HOME/.profile

# xrandr script(generated with arandr)
$HOME/.screenlayout/display.sh

# background (subshell so x session doesn't get initial theme values.)
(
	. ~/.bspwm_theme
	$BG_COMMAND
) &

MPDCRON_DIR="$HOME/.config/mpd"
[ -z $(pgrep -xU $UID mpdcron) ] && mpdcron &

for launch in \
	"xrdb -load $HOME/.Xresources" \
	"xmodmap $HOME/.Xmodmap" \
	"compton" \
	"unclutter" \
	"pcmanfm -d" \
	"mpd $HOME/.config/mpd/mpd.conf" \
	"dunst" \
	"wmname LG3D" \
	"xsetroot -cursor_name left_ptr"
	do eval $launch &
done

# ugh
alias java='java -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
export _JAVA_AWT_WM_NONREPARENTING=1

export SXHKD_SHELL=sh
export xstart=`date +%s`

# launched by display manager bspwm desktop entry.
# sxhkd -c $HOME/.config/sxhkd/sxhkdrc &
exec $HOME/.wm/wm
