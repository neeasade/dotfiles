~/.screenlayout/display.sh

#pcmanfm daemon
pcmanfm -d &

#Set wallpaper
hsetroot -solid "#2ac4ba" &

#Refresh Xresources
xrdb -load  /home/$USER/.Xresources &

#keybindings
xbindkeys &

urxvtd &

#compositing
compton -Ccb -o.9 -f -D 5 -e0.7 &

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
