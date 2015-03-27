Neeasade's dotfiles
===================

##Information
*   WM - bspwm
*   Panel - lemonbar for with slants and xft support
*   term emulator - urxvt
*   font - Dejavu Sans Mono
[imgur album](http://imgur.com/a/SkD5f)
![Screenshot](http://i.imgur.com/8tTVtjx.png)

##TODO
*   add base dotfiles
*   better document things within the dotfiles themselves

##Multihead
6 desktops are made per monitor, and super + # will focuse on that desktop. A panel is created per desktop, with workspace status for that monitor. Additionally, the title on a monitor is for the last active window on a desktop, as determined by a change in bspc window focus history. This behavior can be changed in the title.sh script.

##Tabbing
One feature of this setup is a 'tabbed' display in bar when a desktop is in monocle mode. The active window title is highlighted and the other window titles are clickable on the bar to focus them, though you may also cycle through them with the appropriate key combination. This idea came after a user complained about no i3-like tab mode in bspwm in a thread discussing bspwm's flexibility.

##Dependencies
The programs used here are located in the depends.txt file. You could install them all at once with something like:
```
for i in $(cat depends.txt); do <your package manager install command here> $i; done
```
this is independent per target directory. This list has only been tested with Arch Linux and there are a number of packages that live in the Aur. Also, there are configuration files for programs that are not in te depends.txt with the idea that you only need to install what's used(this primarily affects WM choice, bspwm is installed but i3 and openbox are not in that list, even though there are configuration files for them.)


##Management
These files are managed with GNU [stow](http://www.gnu.org/software/stow/manual/stow.html), which should be available via your package manager of choice. Stow is a symlink-farm management program. It allows for mass symlinking out of a 'master' directory(Here you can see I use the users home directory and the base('/') directory. The deploy scripts in these directories run stow with a target parent directory indicated by name. if you are reading this you are probably interested in only the dotfiles for your home directory. The deploy scripts handle file conflict, stopping if there is an existing file in a place where a symlink would go. If the force flag is set with a deploy script and it finds conflicts, it will store the original files in a directory here should you still need them.

##Misc:
*   The philosophy for the prompt was taken from [dcat](http://dcat.iotek.org/prompt/)
*   Bulk of the information in the bar is gathered by [z3bra's](http://z3bra.org) example
*   the compton setting is modified from [dkeg's](https://bitbucket.orgdkeg/current/src/) config

