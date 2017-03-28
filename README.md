## Neeasade's dotfiles

[screenshot](https://u.teknik.io/3924Rl.png) | [webm](https://u.teknik.io/wk6knx.webm)
---------|------
OS		 | Nixos
WM		 | BSPWM
Panel	 | lemonbar (xft patch)
Terminal | [xst](https://github.com/neeasade/xst)
GTK 	 | [oomox](https://github.com/actionless/oomox) with [ACYL](http://pobtott.deviantart.com/art/Any-Color-You-Like-175624910) icons.
Font 	 | Droid Sans Mono/Tewi

The `meta` folder contains:
- dependencies
- deploy script
- root level dotfiles
- scripts for docs
- todo list

I use GNU stow to manage my dotfiles, To see existing file conflicts if you were to link these files: `stow  */ -t "$HOME" -n 2>&1 | grep -oE ":.+" | cut -c3-`
