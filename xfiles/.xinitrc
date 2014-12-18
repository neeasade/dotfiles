~/.screenlayout/display.sh 

#pcmanfm daemon
pcmanfm -d &

#Set wallpaper
#hsetroot -tile ~/.config/background/background.png &
~/.fehbg

#Refresh Xresources
xrdb -load  /home/neeasade/.Xresources &

#keybindings
xbindkeys &

urxvtd &

#compositing
compton -cb -o0 &

#mpd
mpd ~/.config/mpd/mpd.conf &

#Add panel scripts:
export PATH=$PATH:~/.config/bar

#check if existing socket exists and killl for bspwm(doesn't play nice with logging out and in)
rm /tmp/$(ls /tmp | grep bspwm)

#start bspwm and sxhkd
#sxhkd -f 100 -c /home/neeasade/.config/sxhkd/sxhkdrc &
#exec bspwm -c /home/neeasade/.config/bspwm/bspwmrc #> .xinit.log
exec i3
