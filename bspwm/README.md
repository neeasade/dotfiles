## bspwm

### Multihead
5 desktops are made per monitor, and super + # will focuse on that desktop for the current monitor. A panel is created per desktop, with workspace status for that monitor. Additionally, the title on a monitor is for the last active window on a desktop, as determined by a change in bspc window focus history. This behavior can be changed in the title.sh script. Windows can be moved across monitors and workspaces in an i3-like fashion, see [this post](http://blog.neeasade.net/2015/04/28/BSPWM-Multihead.html) for more details.

### Tabbing
One feature of this setup is a 'tabbed' display in bar when a desktop is in monocle mode. The active window title is highlighted and the other window titles are clickable on the bar to focus them, though you may also cycle through them with the appropriate key combination. This idea came after a user complained about no i3-like tab mode in bspwm in a thread discussing bspwm's flexibility. Optionally, desktops may always be 'tabbed', resulting lemonbar will show titles of all programs in a desktop view. This is only toggled by watching bspwm's window history, so title mode will only change on a change of window focus, sadly.

### Themes
My WM setup is theme oriented, with the current 'theme file' being symlinked to ~/.bspwm_theme. These are located here in the ~/.config/bspwm/themes folder. All that these theme files do is set environment variables that are used by my bspwmrc, panel scripts, and the ltheme script. Many of the colors are pulled from the current loaded termite config colors. 

(todo: gentable for base bspwm theme default values.)
