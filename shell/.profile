PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/.wm/scripts

export EDITOR='vim'
export GIT_EDITOR='/usr/bin/vim'
export BROWSER=qutebrowser
export TERMINAL=termite

# Avoid duplicates
export HISTCONTROL=ignoredups:erasedups

# When the shell exits, append to the history file instead of overwriting it
# (bash)
#shopt -s histappend

# Yes.
export HISTFILESIZE=6000
export HISTSIZE=6000

export HISTFILE="$HOME/.history"
