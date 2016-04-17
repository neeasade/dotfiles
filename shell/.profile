PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/.wm/scripts
PATH=$PATH:$HOME/.gem/ruby/2.3.0/bin

export EDITOR='vim'
export GIT_EDITOR='/usr/bin/vim'
export BROWSER=qutebrowser
export TERMINAL=st

# set GTK+ env var for GTK3 programs (allows setting without global theme setting)
# TODO: find a way to fork this quietly on shell start, don't like the idea of IO in ~/.profile.
export GTK_THEME="`cat $HOME/.themes/ACTIVE_THEME 2>/dev/null`" >/dev/null

# Avoid duplicates
export HISTCONTROL=ignoredups:erasedups

# When the shell exits, append to the history file instead of overwriting it
# (bash)
#shopt -s histappend

# Yes.
export HISTFILESIZE=6000
export HISTSIZE=6000

export HISTFILE="$HOME/.history"
