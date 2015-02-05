Neeasade's dotfiles
===================

##Information
*   WM - bspwm
*   term emulator - urxvt
*   font - tewi
[imgur album](http://imgur.com/a/hYQkg)
![Screenshot](http://i.imgur.com/ceexx19.png)

##Multihead
Currently one bar is made per monitor, with the same information being shown on each with the exception of the workspaces, those that are shown are monitor specific.

##TODO
*   add base dotfiles
*   document things within the dotfiles themselves
*   improve the deploy scripts to back up existing files and be 'nice'

##Dependencies
The programs used here are located in the depends.txt file. You could install them all at once with something like:
```
for i in $(cat depends.txt); do <your package manager install command here> $i; done
```
this is independent per target directory. This list has only been tested with Arch Linux and there are a number of packages that live in the Aur. Also, there are configuration files for programs that are not in te depends.txt with the idea that you only need to install what's used(this primarily affects WM choice, bspwm is installed but i3 and openbox are not in that list, even though there are configuration files for them.)


##Management
These files are managed with GNU [stow](http://www.gnu.org/software/stow/manual/stow.html), which should be available via your package manager of choice. Stow is a symlink-farm management program. It allows for mass symlinking out of a 'master' directory(Here you can see I use the users home directory and the base('/') directory. The deploy scripts in these directories run stow with a target parent directory indicated by name. if you are reading this you are probably interested in only the dotfiles for your home directory.

##Misc:
*   The philosophy for the prompt was taken from [dcat](http://dcat.iotek.org/prompt/)
*   Bulk of the information in the bar is gathered by [z3bra's](http://z3bra.org) example
*   the compton setting is modified from [dkeg's](https://bitbucket.orgdkeg/current/src/) config

