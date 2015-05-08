Neeasade's dotfiles
===================

##Information
*   WM - bspwm
*   Panel - lemonbar branch with xft support, from the lemonbar-xft-git aur package
*   term emulator - urxvt
*   font - Dejavu Sans Mono

[Imgur album](http://imgur.com/a/SkD5f) - [Multihead picture](http://a.pomf.se/poderp.png) - [Webm](http://a.pomf.se/tzjbxu.webm)
![Screenshot](http://i.imgur.com/8tTVtjx.png)

##TODO(ricing roadmap)
*   add more root dotfiles
*   nvim
*   gtk themes (with matching colors)
*   more bspwm features and keybinds(cut and paste a desktop)
*   clean tmux.conf, sync keybinds for copy/paste/other with urxvt (passthrough to term)
*   better document and 'clean' things within the dotfiles themselves(never ends)

##Multihead
5 desktops are made per monitor, and super + # will focuse on that desktop for the current monitor. A panel is created per desktop, with workspace status for that monitor. Additionally, the title on a monitor is for the last active window on a desktop, as determined by a change in bspc window focus history. This behavior can be changed in the title.sh script. Windows can be moved across monitors and workspaces in an i3-like fashion, see [this post](http://blog.neeasade.net/2015/04/28/BSPWM-Multihead.html) for more details.

##Tabbing
One feature of this setup is a 'tabbed' display in bar when a desktop is in monocle mode. The active window title is highlighted and the other window titles are clickable on the bar to focus them, though you may also cycle through them with the appropriate key combination. This idea came after a user complained about no i3-like tab mode in bspwm in a thread discussing bspwm's flexibility. Optionally, desktops may always be 'tabbed', resulting lemonbar will show titles of all programs in a desktop view.

##Dependencies
The programs used here are located in the depends.txt file. You could install them all at once with something like:
```
for i in $(cat depends.txt); do <your package manager install command here> $i; done
```
this is independent per target directory. This list has only been tested with Arch Linux and there are a number of packages that live in the AUR. I have tried to include only packages that make up most of the WM experience(for example ranger and qutebrowser are not in the depends.txt)
There is a script at the top level named setup.sh - this will populate any git submodules(eg for vim/tmux/other package managers) and also install everything in the depends.txt files.

##Management
These files are managed with GNU [stow](http://www.gnu.org/software/stow/manual/stow.html), which should be available via your package manager of choice. Stow is a symlink-farm management program. It allows for mass symlinking out of a 'master' directory(Here you can see I use the users home directory and the base('/') directory. The deploy scripts in these directories run stow with a target parent directory indicated by name. if you are reading this you are probably interested in only the dotfiles for your home directory. The deploy scripts handle file conflict, stopping if there is an existing file in a place where a symlink would go. If the force flag is set with a deploy script and it finds conflicts, it will store the original files in a directory here should you still need them.

##Misc:
*   The philosophy for the prompt was taken from [dcat](http://dcat.iotek.org/prompt/)
*   Initial information(and still some) displayed in the bar is from [z3bra's](http://z3bra.org) example
*   the compton setting is modified from [dkeg's](https://bitbucket.org/dkeg/current/src/) config

