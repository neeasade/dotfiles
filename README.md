Neeasade's dotfiles
===================

##Information
*   WM - bspwm
*   term emulator - urxvt
*   font - termsyn
[imgur album](http://imgur.com/a/hYQkg)
![Screenshot](http://i.imgur.com/ceexx19.png)

##Multihead
Currently one bar is made per monitor, with the same information being shown on each with the exception of the workspaces, those that are shown are monitor specific.

##TODO
*   better multihead support
*   add information to bar(currently there is only a clock, xtitle, and workspace indicators
*   better fallback support(eg font fallback in Xresources)
*   change bar to XFT bar for and use pretty icons n shit in the bar
*   add base dotfiles
*   document things within the dotfiles themselves

##Dependencies
The programs used here are located in the depends.txt file. You could install them all at once with something like:
```
for i in $(cat depends.txt); do <your package manager install command here> $i; done
```
this is independent per target directory.


##Management
These files are managed with GNU [stow](http://www.gnu.org/software/stow/manual/stow.html), which should be available via your package manager of choice. Stow is a symlink-farm management program. It allows for mass symlinking out of a 'master' directory(Here you can see I use the users home directory and the base('/') directory. The deploy scripts in these directories run stow with a target parent directory indicated by name. if you are reading this you are probably interested in only the dotfiles for your home directory.

##Misc:
*   The philosophy for the prompt was take from [dcat](http://dcat.iotek.org/prompt/)

