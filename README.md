Neeasade's dotfiles
===================

Information | -
------------|---
WM | bspwm
Panel | lemonbar (xft patch)
Terminal Emulator | termite, for it's live reload of configuration feature.
GTK theme | [oomox](https://github.com/actionless/oomox) with [ACYL](http://pobtott.deviantart.com/art/Any-Color-You-Like-175624910) icons.
Font | Droid Sans Mono

Further details can be found within the folders here themselves, at a top level. The README's assume you are familiar with the tool/program you are reading about.

[Workflow webm](https://u.teknik.io/wk6knx.webm)
![Screenshot](https://u.teknik.io/si6yKy.png)

### TODO(ricing roadmap)
*   rewrite panel in python
*   learn emacs
*   turn this repo into a template to be used transparently with something like http://github.com/neeasade/autotheme.sh [done]
	* Write a package build for urnn and improve the dataset for this.
	* Consider making oomox a dependency and having the setup script generate the gtk themes.
*   more BSPWM window manuevers - json possiblities?
*   get used to mutt/run in a tmux session.
*   investigate more thoroughly inline reloading possibilities of urxvt or st.
*   run BFG repo cleaner on this repo
*   document the bspwm/gtk/bin/root/mutt directories
*   better document and 'clean' things within the dotfiles themselves(never ends)

### Dependencies
Most programs used here are located in the depends.txt file. You could install them all at once with something like:
```
for i in $(cat depends.txt); do <your package manager install command here> $i; done
```
This list has only been tested with Arch Linux and there are a number of packages that live in the AUR. There is a script at the top level named setup.sh - this will populate any git submodules(eg for vim/tmux/other package managers) and also install everything in the depends.txt files (assumes yaourt).

### Management
Excluding the root folder, all of these files are meant to be symlinked to a users home folder, most likely stored in something like ~/.dotfiles. I deploy these with GNU [stow](http://www.gnu.org/software/stow/manual/stow.html):
```
stow $(ls */ -d | grep -v root)
```
If there are any conflicts, files will not be symlinked and you will be told about conflicts. The following shows conflicts parsed out of stows output, without actually linking the files:
```
stow -n $(ls */ -d | grep -v root) 2>&1 | grep -oE ":.+" | cut -c3-
```

### Misc:
*   The philosophy for the prompt was taken from [dcat](http://dcat.iotek.org/prompt/)
*   Initial information(and still some) displayed in the bar is from [z3bra's](http://z3bra.org) example
*   the compton setting is modified from [dkeg's](https://bitbucket.org/dkeg/current/src/) config
