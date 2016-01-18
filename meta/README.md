### Todo

*   rewrite panel in python
*   learn emacs
*   more BSPWM window manuevers
    * look into json opporunities for i3-like window frame movement
    * floating keybinds(move, resize, better focus)
*   get used to mutt/run in a tmux session.
*   investigate more thoroughly inline reloading possibilities of urxvt or st.
*   run BFG repo cleaner on this repo
*   document the bspwm/gtk/bin/root/mutt directories
    * don't forget to credit dcat for initial prompt, and earsplit/z3bra for initial panel scripts.
*   make a new workflow webm.
*   pulse.sh border correction on change window
    * see if can use more bspc commands here
*   https://gist.github.com/romainl/5cd2f4ec222805f49eca
*   incorporate automagic stuff
*   use mustache templates in this fashion - https://github.com/rcrowley/mustache.sh
*   retest deploy script with restructure.
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
