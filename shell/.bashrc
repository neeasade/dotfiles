# If not running interactively, don't do anything
[[ $- != *i* ]] && return

. $HOME/.profile
PS1='$(prompt) '


[ -f ~/.fzf.bash ] && . ~/.fzf.bash || true
