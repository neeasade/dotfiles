#!/usr/bin/env bash

# Load RVM, if you are using it
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm

# Add rvm gems and nginx to the path
export PATH=$PATH:~/.gem/ruby/1.8/bin:/opt/nginx/sbin

# Your place for hosting Git repos. I use this for private repos.
export GIT_HOSTING='nathanisom27@bitbucket.com'

# Set my editor and git editor
export EDITOR="/usr/bin/gvim"
export GIT_EDITOR='/usr/bin/gvim'

# Set the path nginx
export NGINX_PATH='/opt/nginx'

# Don't check mail when opening terminal.
unset MAILCHECK

export IRC_CLIENT='irssi'

# Set this to the command you use for todo.txt-cli

export TODO="~/Documents/todo.txt"

source ~/.bashrc
