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

export PS1="┌─[\u][\w]\n└─╼"

#set a mobile variable for differences in configuration between mobile/desktop.
m=false
case "HOME9" in
  $HOTSNAME) m=true ;;
esac
