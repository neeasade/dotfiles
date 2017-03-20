# If not running interactively, don't do anything
[[ $- != *i* ]] && return

. $HOME/.profile
set_history
PS1='$(prompt)'


[ -f ~/.fzf.bash ] && . ~/.fzf.bash || true
