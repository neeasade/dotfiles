#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Make sure profile gets sourced.
. $HOME/.profile

# auto-complete for pacman when using sudo:
complete -cf sudo

# functions
function swap() {
    # Swap 2 filenames around, if they exist (from Uzi's bashrc).
    local TMPFILE=tmp.$$

    [ $# -ne 2 ] && echo "swap: 2 arguments needed" && return 1
    [ ! -e $1 ] && echo "swap: $1 does not exist" && return 1
    [ ! -e $2 ] && echo "swap: $2 does not exist" && return 1

    mv "$1" $TMPFILE
    mv "$2" "$1"
    mv $TMPFILE "$2"
}

function extract()      # Handy Extract Program
{
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xvjf $1     ;;
            *.tar.gz)    tar xvzf $1     ;;
            *.bz2)       bunzip2 $1      ;;
            *.rar)       unrar x $1      ;;
            *.gz)        gunzip $1       ;;
            *.tar)       tar xvf $1      ;;
            *.tbz2)      tar xvjf $1     ;;
            *.tgz)       tar xvzf $1     ;;
            *.zip)       unzip $1        ;;
            *.Z)         uncompress $1   ;;
            *.7z)        7z x $1         ;;
            *)           echo "'$1' cannot be extracted via >extract<" ;;
        esac
    else
        echo "'$1' is not a valid file!"
    fi
}

prompt () {
    _ERR=$?
    _JOB=$(jobs | wc -l)

    [ $_ERR -ne 0 ] && echo -e -n '\e[7m' # invert
    [ $_JOB -ne 0 ] && echo -n "$"
    echo -n "$"
    echo -e -n '\e[0m' # reset
}

PS1='$(prompt) '

# aliases
alias tmux='tmux -2' #Make tmux assume 256 colors.
alias cavampd='cava -i fifo -p /tmp/mpd.fifo -b 20'
alias sysinfo='archey3 && dfc -p /dev && colors'
alias ls='ls --color=auto'
alias paste="curl -F 'sprunge=<-' http://sprunge.us"
alias grep="grep --color=auto"
alias pacman="pacman --color=always"
alias make="clear && make"
alias shot="scrot ~/Screenshots/`date +%y-%m-%d-%H:%M:%S`.png"
alias getip="curl -s checkip.dyndns.org | sed -e 's/.*Current IP Address: //' -e 's/<.*$//'"

# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() {
  local file
  file=$(fzf --query="$1" --select-1 --exit-0)
  [ -n "$file" ] && ${EDITOR} "$file"
}
export FZF_CTRL_T_COMMAND=fe

# autostartx if running on the first tty:
if [[ -z $DISPLAY && $XDG_VTNR -eq 1 && -z $TMUX ]]; then exec startx; fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
