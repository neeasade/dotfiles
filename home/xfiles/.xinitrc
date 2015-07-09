#Xrandr script:
~/.screenlayout/display.sh

#pcmanfm daemon - useful for automatic mounting of drives
pcmanfm -d &

#load Xresources
xrdb -load  /home/$USER/.Xresources &

#background
~/.fehbg &

#compositing
compton &

#mpd and mpdcron(for notifications and lastfm scrobbling)
mpd ~/.config/mpd/mpd.conf &
[[ -z $(pgrep -xU $UID mpdcron) ]] && mpdcron &

#make the default x cursor not an x :)
xsetroot -cursor_name left_ptr &

#dunst
. ~/.bspwm_theme
dunst -geometry 200x200-10-10 \
    -fn "$DUNST_FONT" \
    -lb "$defaultBG" -lf "$defaultFG" \
    -nb "$defaultBG" -nf "$defaultFG" \
    -cb "$defaultBG" -cf "#ffff00" &

#check if existing socket exists and killl for bspwm(doesn't play nice with logging out and in)
rm /tmp/$(ls /tmp | grep bspwm)

#set shell that sxhkd uses to execute commands:
export SXHKD_SHELL=sh

#start bspwm and sxhkd
sxhkd -f 100 -c /home/$USER/.config/sxhkd/sxhkdrc &
exec bspwm -c /home/$USER/.config/bspwm/bspwmrc > .xinit.log
