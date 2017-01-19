### Todo

*   rewrite panel in something [lemonade?](https://github.com/neeasade/lemonade)
    *   step one: recreate existing schemes with this
    *   consider combining with [slants](https://github.com/neeasade/lemonbar) (would imply packaging)
*   more BSPWM window manuevers
    * better keybinds (able to mash a direction and hit parent nodes in that direction if deadend with nodes)
*   get used to mutt/run in a tmux session.
*   run BFG repo cleaner on this repo
    *   figure out background storage first.
*   make a new workflow webm.
*   incorporate automagic stuff (a 'random' theme that pulls from unsplash.it and autothemes)
    * package urnn-git first.
*   window decorations w/ lemonbar dickery
*   scratchpad handling (bspwm)
*   better document and 'clean' things within the dotfiles themselves(never ends)

use this again somewhere(underline style block):
```
function block() {
    [ ! -z $NoPadding ] && pPadding=$NoPadding
    if [ "$blockActive" = true ] ; then
        echo -n "%{B#ff$(colort 2 $(echo $pBG | cut -c4-))}%{-o}%{U$pBGActiveTab+u}%{F$pFG}$(printf %${pPadding}s)$@$(printf %${pPadding}s)%{B$pBG}%{-u} "
    else
        echo -n "%{-o}%{U#ff$(colort -3 $(echo $pBGActiveTab | cut -c4-))+u}%{F$pFG}$(printf %${pPadding}s)$@$(printf %${pPadding}s)%{B$pBG}%{-u} "
    fi
}
export -f block
```
