#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

PATH=$PATH:~/bin:~/.config/bar

#add the dir for gem execs to path:
PATH=$PATH:/home/neeasade/.gem/ruby/2.2.0/bin

#auto-complete for pacman when using sudo:
complete -cf sudo

#export PS1="┌─[\u][\w]\n└─╼"
#export PS1="───"

prompt () {
	_ERR=$?
	_UID=$(id -u)
	_JOB=$(jobs | wc -l)

	[ $_UID -eq 0 ] && echo -n '━' || echo -n -e '─'
	[ $_JOB -ne 0 ] && echo -n '!' || echo -n -e '─'
	[ $_ERR -ne 0 ] && echo -n '!' || echo -n -e '─'
}

PS1='$(prompt) '


