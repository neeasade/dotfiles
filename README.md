## Neeasade's dotfiles

[screenshot](https://u.teknik.io/3924RL.png) | [webm](https://u.teknik.io/wk6knx.webm)
---------|------
OS		 | Arch Linux
WM		 | BSPWM
Panel	 | lemonbar (xft patch)
Terminal | termite, for it's live reload of configuration feature.
GTK 	 | [oomox](https://github.com/actionless/oomox) with [ACYL](http://pobtott.deviantart.com/art/Any-Color-You-Like-175624910) icons.
Font 	 | Droid Sans Mono/Tewi

Further details can be found within the folders here themselves, at a top level. The README's assume you are familiar with the tool/program you are reading about, and describe how I use them in my setup.

The `meta` folder contains dependencies, a deploy script, a todo list and some scripts to generate tables seen here.

I use GNU stow to manage my dotfiles, To see existing file conflicts if you were to link these files: `stow -t "$HOME" -n 2>&1 | grep -oE ":.+" | cut -c3-`
