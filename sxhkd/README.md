## SXHKD

Here is a generated table of the bindings I use (in progress)


key | bind
----|-----
super + c | `     echo "$(bspc query -D -d)" > /tmp/bspwmdtop_swap `
super + v | `	 for win in $(bspc query -N -d $(cat /tmp/bspwmdtop_swap)); do   `<br>`	     bspc node $win -d $(bspc query -D -d);    `<br>`     done  `
super + w | `     bspc node -c `
super + t | `     bspc desktop -l next && bspc node -f next `
super + b | `     bspc desktop -B `
super + s | `	 bspc query -N -n focused.tiled && state=floating || state=tiled;   `<br>`     bspc node -t $state  `
super + f | `	 state=fullscreen;   `<br>`	 bspc query -N -n "focused.$state" && state=$(bspc query -T -n | jshon -e client -e lastState -u);    `<br>`     bspc node -t "$state"  `
super + {_,shift} + Tab | `     bspc desktop -f {next,prev} `
alt + {_,shift} + Tab | `     bspc node -f {next,prev} `
super + apostrophe | `     bspc node -s last `
super + m | `     bspc node -s biggest `
super + shift + {h,j,k,l} | `	 bspc config pointer_follows_focus true;   `<br>`	 cur_win=$(bspc query -N -n);	`<br>`	 cur_mon=$(bspc query -M -m);	`<br>`	 dir={west,south,north,east};	`<br>`	 if ! bspc node -f $dir; then	`<br>`		 bspc node -m $dir;	`<br>`		 bspc monitor -f $dir;	`<br>`	 else    `<br>`         new_mon=$(bspc query -M -m);    `<br>`         if [ $new_mon -eq $cur_mon ]; then    `<br>`             bspc node -s $cur_win;    `<br>`         else    `<br>`             bspc node $cur_win -m ^$new_mon;    `<br>`         fi;    `<br>`         bspc node -f $cur_win;    `<br>`     fi;    `<br>`     bspc config pointer_follows_focus false  `
super +  {h,j,k,l} | `	 bspc config pointer_follows_monitor true;   `<br>`	 bspc config pointer_follows_focus true;	`<br>`	 dir={west,south,north,east};	`<br>`	 if ! bspc node -f $dir; then	`<br>`         bspc monitor -f $dir;    `<br>`     fi;    `<br>`     bspc config pointer_follows_monitor false;    `<br>`     bspc config pointer_follows_focus false  `
super + {comma,period} | `     bspc desktop -C {backward,forward} `
super + ctrl + {h,j,k,l} | `     bspc node -p {west,south,north,east} `
super + ctrl + {_,shift + }space | `     bspc node @{_,/} -p cancel `
super + alt + {h,j,k,l} | `     bspc node {@west -r -35,@south -r +35,@north -r -35,@east -r +35} `
super + alt + shift + {h,j,k,l} | `     bspc node {@east -r -35,@north -r +35,@south -r -35,@west -r +35} `
super + ctrl + {1-9} | `     bspc node -r 0.{1-9} `
super + {1-9} | `	 D={1-9};   `<br>`     bspc desktop -f "$(bspc query -D -m | sed -n "$D p")"  `
super + shift + {1-9} | `	 D={1-9};   `<br>`     bspc node -d "$(bspc query -D -m | sed -n "$D p")"  `
~button1 | `     bspc pointer -g focus `
super + button{1-3} | `     bspc pointer -g {move,resize_side,resize_corner} `
super + !button{1-3} | `     bspc pointer -t %i %i `
super + @button{1-3} | `     bspc pointer -u `
super + shift + q | `	 pgrep lemonbar && ~/bin/panelt;   `<br>`	 for win in $(bspc query -N); do bspc node $win -c ; done;    `<br>`     bspc quit  `
super + {_,shift} + semicolon | `     bspc node @/ -R {90,270} `
super + {_,shift} + slash | `     ~/bin/{gapt,panelt} `
super + p | `     i3blur.sh `
super + o | `     $BROWSER `
super + e | `     pcmanfm `
super + Return | `     $TERMINAL `
super + space | `     eval dmenu_run $(dmenu_options) `
super + shift + space | `     eval passmenu $(dmenu_options) `
XF86Audio{Prev,Next} | `      mpc -q {prev,next} `
XF86AudioPlay | `      mpc -q toggle `
XF86Audio{LowerVolume,RaiseVolume} | `     amixer -q sset Master 3%{-,+} `
XF86AudioMute | `     amixer -q set Master toggle `
