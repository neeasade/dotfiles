#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

PATH=$PATH:~/bin:~/.config/bar

#add the dir for gem execs to path:
PATH=$PATH:/home/neeasade/.gem/ruby/2.1.0/bin

#auto-complete for pacman when using sudo:
complete -cf sudo

#export PS1="┌─[\u][\w]\n└─╼"
#export PS1="───"

#set a mobile variable for differences in configuration between mobile/desktop.
m=false
case "home9" in
  $HOSTNAME) m=true ;;
esac

prompt () {
	_ERR=$?
	_UID=$(id -u)
	_JOB=$(jobs | wc -l)

	[ $_UID -eq 0 ] && echo -n '[31m━' || echo -n '[30m─'
	[ $_JOB -ne 0 ] && echo -n '[32m─' || echo -n '[30m─'
	[ $_ERR -ne 0 ] && echo -n '[33m─' || echo -n '[30m─'
	
	echo -n '[0m'
}

PS1='$(prompt) '


