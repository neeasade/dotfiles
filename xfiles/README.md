## xfiles

The only file of relavence here is ~/.xinitrc, which is called upon startx. ~/.xprofile is called by several display managers, so I have made mine just call the xinitrc so I can manage one file.

my ~/.xinitrc performs the following:
- launches pcmanfm daemon
- loads xresources
- sets background based on variable set in ~/.bspwm_theme
- launches compton
- launches mpd and mpdcron if it is not running.
- makes the default cursor not an 'X'
- launch dunst with values sourced from ~/.bspwm_theme
- unsets the blog function
- remove bspwm socket if it exists
- launch bspwm and sxhkd
