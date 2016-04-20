### Todo

*   rewrite panel in something [lemonade?](https://github.com/neeasade/lemonade)
    *   step one: recreate existing schemes with this
    *   consider combining with [slants](https://github.com/neeasade/lemonbar) (would imply packaging)
*   nvim plugins (deoplete)
*   more BSPWM window manuevers
    * better keybinds (able to mash a direction and hit parent nodes in that direction if deadend with nodes)
    * floating keybinds(move, resize, better focus)
*   get used to mutt/run in a tmux session.
*   run BFG repo cleaner on this repo
*   make a new workflow webm.
*   handle conditional true color nvim setting
*   https://gist.github.com/romainl/5cd2f4ec222805f49eca ? someway to auto-vim colors.
*   incorporate automagic stuff (a 'random' theme that pulls from unsplash.it and autothemes)
    * package urnn-git first.
*   learn spacemacs?
*   window decorations w/ lemonbar dickery
*   scratchpad handling (bspwm)
*   mess with node hidden flags (bspwm)
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
