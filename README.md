Neeasade's dotfiles
===================

##Information
*   WM - bspwm
*   Panel - lemonbar with xft support, from the lemonbar-xft-git aur package
*   term emulator - termite, for it's live reload of configuration feature.
*   Multiple themes are used here(see below)
*   GTK theme - FlatStudioDark/Dorian-Slate/zenburn
*   font - Dejavu Sans Mono/Droid Sans/Terminus

[Workflow webm](https://u.teknik.io/wk6knx.webm)
![Screenshot](https://u.teknik.io/si6yKy.png)

##TODO(ricing roadmap)
*   rewrite panel in python
*   learn emacs
*   turn this repo into a template to be used transparently with something like http://github.com/neeasade/autotheme.sh
	* Write a package build for urnn and improve the dataset for this.
	* Consider making oomox a dependency and having the setup script generate the gtk themes.
*   more BSPWM window manuevers - json possiblities?
*   better document and 'clean' things within the dotfiles themselves(never ends)

##Multihead
5 desktops are made per monitor, and super + # will focuse on that desktop for the current monitor. A panel is created per desktop, with workspace status for that monitor. Additionally, the title on a monitor is for the last active window on a desktop, as determined by a change in bspc window focus history. This behavior can be changed in the title.sh script. Windows can be moved across monitors and workspaces in an i3-like fashion, see [this post](http://blog.neeasade.net/2015/04/28/BSPWM-Multihead.html) for more details.

##Tabbing
One feature of this setup is a 'tabbed' display in bar when a desktop is in monocle mode. The active window title is highlighted and the other window titles are clickable on the bar to focus them, though you may also cycle through them with the appropriate key combination. This idea came after a user complained about no i3-like tab mode in bspwm in a thread discussing bspwm's flexibility. Optionally, desktops may always be 'tabbed', resulting lemonbar will show titles of all programs in a desktop view. This is only toggled by watching bspwm's window history, so title mode will only change on a change of window focus, sadly.

##Themes
This setup bases most all it's settings on a theme file, ~/.bspwm_theme. It is set up to allow multiple themes with different files that get symlinked to that location. I use termite to transition through configs on the fly, as seen in the workflow webm.

##Dependencies
Most programs used here are located in the depends.txt file. You could install them all at once with something like:
```
for i in $(cat depends.txt); do <your package manager install command here> $i; done
```
This list has only been tested with Arch Linux and there are a number of packages that live in the AUR. There is a script at the top level named setup.sh - this will populate any git submodules(eg for vim/tmux/other package managers) and also install everything in the depends.txt files(assumes yaourt).

##Management
Excluding the root folder, all of these files are meant to be symlinked to a users home folder, most likely stored in something like ~/.dotfiles. I deploy these with GNU [stow](http://www.gnu.org/software/stow/manual/stow.html):
```
stow $(ls */ -d | grep -v root)
```
If there are any conflicts, files will not be symlinked and you will be told about conflicts. The following shows conflicts parsed out of stows output:
```
stow -n $(ls */ -d | grep -v root) 2>&1 | grep -oE ":.+" | cut -c3-
```

##I want this on my computer and I run Arch Linux.
Cool, assuming you have git and yaourt, run the following.
```
git clone http://github.com/neeasade/dotfiles ~/.dotfiles_neeasade && ~/.dotfiles_neeasade/deploy.sh
```
I take no responsibility for what happens when you do this. Any conflicting files will be moved to ~/dotfile_conflicts

##Misc:
*   The philosophy for the prompt was taken from [dcat](http://dcat.iotek.org/prompt/)
*   Initial information(and still some) displayed in the bar is from [z3bra's](http://z3bra.org) example
*   the compton setting is modified from [dkeg's](https://bitbucket.org/dkeg/current/src/) config
