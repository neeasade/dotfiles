PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/.wm/scripts
PATH=$PATH:$HOME/.gem/ruby/2.3.0/bin

export EDITOR='vim'
export GIT_EDITOR='/usr/bin/vim'
export BROWSER=qutebrowser
export TERMINAL=st

# Avoid duplicates
export HISTCONTROL=ignoredups:erasedups

# When the shell exits, append to the history file instead of overwriting it
# (bash)
#shopt -s histappend

# Yes.
export HISTFILESIZE=6000
export HISTSIZE=6000

export HISTFILE="$HOME/.history"

#export LD_PRELOAD=/usr/lib/gtk3-nocsd/gtk3-nocsd.so
