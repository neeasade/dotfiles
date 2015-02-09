~/.screenlayout/display.sh

#pcmanfm daemon
pcmanfm -d &

#Set wallpaper
hsetroot -tile ~/.background.png -tint "#00ee55" &

#Refresh Xresources
xrdb -load  /home/$USER/.Xresources &

#keybindings
xbindkeys &

urxvtd &

#compositing
compton &

#mpd
mpd ~/.config/mpd/mpd.conf &

#Add panel scripts:
export PATH=$PATH:~/.config/bar

#check if existing socket exists and killl for bspwm(doesn't play nice with logging out and in)
rm /tmp/$(ls /tmp | grep bspwm)

xsetroot -cursor_name left_ptr &

#start bspwm and sxhkd
sxhkd -f 100 -c /home/neeasade/.config/sxhkd/sxhkdrc &
exec bspwm -c /home/neeasade/.config/bspwm/bspwmrc > .xinit.log
#exec i3
