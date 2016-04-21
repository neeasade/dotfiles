# If not running interactively, don't do anything
[[ $- != *i* ]] && return

. $HOME/.profile
PS1='$(prompt) '

# autostartx if running on the first tty:
if [[ -z $DISPLAY && $XDG_VTNR -eq 1 && -z $TMUX ]]; then exec startx; fi

[ -f ~/.fzf.$0 ] && source ~/.fzf.$0 || true
