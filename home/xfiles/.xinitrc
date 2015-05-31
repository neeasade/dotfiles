#Xrandr script:
~/.screenlayout/display.sh

#pcmanfm daemon - useful for automatic mounting of drives
pcmanfm -d &

#load Xresources
xrdb -load  /home/$USER/.Xresources

#Set background to same color as Xresources background
hsetroot -solid "$(xrdb -query | grep  "background" | grep -oE "#[a-zA-Z0-9]{6}")" &

#urxvt daemon
urxvtd &

#compositing
compton &

#mpd
mpd ~/.config/mpd/mpd.conf &

#check if existing socket exists and killl for bspwm(doesn't play nice with logging out and in)
rm /tmp/$(ls /tmp | grep bspwm)

#make the default x cursor not an x :)
xsetroot -cursor_name left_ptr &

# dunst
. ~/.bspwm_theme
dunst -geometry 200x200-10-10 \
    -fn "$DUNST_FONT" \
    -lb "$defaultBG" -lf "$defaultFG" \
    -nb "$defaultBG" -nf "$defaultFG" \
    -cb "$defaultBG" -cf "#ffff00" &

#set shell that sxhkd uses to execute commands:
export SXHKD_SHELL=sh

#start bspwm and sxhkd
sxhkd -f 100 -c /home/$USER/.config/sxhkd/sxhkdrc &
exec bspwm -c /home/$USER/.config/bspwm/bspwmrc > .xinit.log
