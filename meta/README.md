### Todo

*   rewrite panel in python
*   learn emacs
*   more BSPWM window manuevers
    * look into json opporunities for i3-like window frame movement
    * floating keybinds(move, resize, better focus)
*   get used to mutt/run in a tmux session.
*   run BFG repo cleaner on this repo
*   make a new workflow webm.
*   https://gist.github.com/romainl/5cd2f4ec222805f49eca
*   incorporate automagic stuff
*   better handle GTK_THEME var (not in profile/maybe look into hooking up with dmenu_run only)
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
