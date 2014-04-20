#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

PATH=$PATH:~/bin

#add the dir for gem execs to path:
PATH=$PATH:/home/neeasade/.gem/ruby/2.1.0/bin

#auto-complete for pacman when using sudo:
complete -cf sudo

#generated PS1 from bashrcgenerator.com:

export PS1="\[\e[00;37m\][\u][\w]\n>\[\e[0m\]"
