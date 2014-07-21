~/.screenlayout/display.sh &&

#pcmanfm daemon
pcmanfm -d &

#Set wallpaper
hrootset -tile ~/images/changed.png &

#Refresh Xresources
xrdb -load  /home/neeasade/.Xresources &

#keybindings
xbindkeys &

#compositing
compton -cb -o0 &

#mpd
mpd ~/.config/mpd/mpd.conf & 

#start i3.
exec i3

