#!/usr/bin/env bash

# Load RVM, if you are using it
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm

# Add rvm gems and nginx to the path
export PATH=$PATH:~/.gem/ruby/1.8/bin:/opt/nginx/sbin

# Set my editor and git editor
export GIT_EDITOR='/usr/bin/vim'
export EDITOR=vim
export BROWSER=chromium

# Set the path nginx
export NGINX_PATH='/opt/nginx'

# Don't check mail when opening terminal.
unset MAILCHECK

# Set IRC client
export IRC_CLIENT='irssi'

# Set this to the command you use for todo.txt-cli
export TODO="/home/$USER/.todo.txt"

source ~/.bashrc
