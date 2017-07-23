# source everything
for file in $HOME/.sh.d/*; do
    . $file
done

# autostartx if running on the first tty:
#[[ -z $DISPLAY && $XDG_VTNR -eq 1 && -z $TMUX ]] && exec startx
