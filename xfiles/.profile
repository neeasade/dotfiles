PATH=$PATH:$HOME/bin

export EDITOR='nvim'
export GIT_EDITOR='/usr/bin/nvim'
export BROWSER=chromium

export TODO="$HOME/.todo.txt"

# Avoid duplicates
export HISTCONTROL=ignoredups:erasedups

# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend

# Yes.
export HISTFILESIZE=6000
export HISTSIZE=6000

export HISTFILE="$HOME/.history"
