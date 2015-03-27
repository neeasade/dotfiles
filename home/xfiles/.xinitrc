~/.screenlayout/display.sh

#pcmanfm daemon - useful for automatic mounting of drives
pcmanfm -d &

#Set wallpaper
hsetroot -tile ~/.background.png &

#Refresh Xresources
xrdb -load  /home/$USER/.Xresources &

#urxvt daemon
urxvtd &

#compositing
compton &

#mpd
mpd ~/.config/mpd/mpd.conf &

#Add panel scripts:
export PATH=$PATH:~/.config/bar

#check if existing socket exists and killl for bspwm(doesn't play nice with logging out and in)
rm /tmp/$(ls /tmp | grep bspwm)

#make the default x cursor not an x :)
xsetroot -cursor_name left_ptr &

#start bspwm and sxhkd
sxhkd -f 100 -c /home/$USER/.config/sxhkd/sxhkdrc &
exec bspwm -c /home/$USER/.config/bspwm/bspwmrc > .xinit.log
