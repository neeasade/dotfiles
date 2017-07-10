PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/.gem/bin
PATH=$PATH:$HOME/.wm/scripts/theming
PATH=$PATH:$HOME/.wm/scripts/interact
PATH=$PATH:$HOME/.wm/scripts/visual

# {{{ env
export EDITOR='vim'
export GIT_EDITOR='/usr/bin/vim'
export BROWSER=qutebrowser
export TERMINAL=st
export FILEBROWSER=pcmanfm

export MPDCRON_DIR="$HOME/.config/mpd"
export SXHKD_SHELL=sh
export _JAVA_AWT_WM_NONREPARENTING=1

type npm >/dev/null && export NODE_PATH="$(npm root -g)"
type ruby >/dev/null && export GEM_HOME="$HOME/.gem"

# }}}

# {{{ alias
alias getip="curl -s checkip.dyndns.org | sed -e 's/.*Current IP Address: //' -e 's/<.*$//'"
alias grep="grep --color=auto"
alias java='java -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
alias ls='ls --color=auto'
alias make="clear && make"
alias mpv='~/.wm/scripts/mpv'
alias pacman="pacman --color=always"
alias paste="curl -F 'sprunge=<-' http://sprunge.us"
alias steam-wine='WINEDEBUG=-all wine ~/.wine/drive_c/Program\ Files\ \(x86\)/Steam/Steam.exe >/dev/null 2>&1 &'
alias sysinfo='archey3 && dfc -p /dev && colors'
alias tmux='tmux -2' #Make tmux assume 256 colors.
# }}}

# {{{ func

shot() {
    mkdir -p $HOME/Screenshots
    eval $(slop)
    maim -g ${G} "$HOME/Screenshots/$(date +%y-%m-%d-%H:%M:%S).png"
}

fullshot() {
    mkdir -p $HOME/Screenshots
    maim "$HOME/Screenshots/$(date +%y-%m-%d-%H:%M:%S).png"
}

upshot() {
    shot
    uguush -o 0x0 -u "$(echo $HOME/Screenshots/$(ls $HOME/Screenshots | tail -n 1))"
}

mpv() {
    $(which mpv) --input-unix-socket=/tmp/mpvsocket "$*"
}

# nix query
nq () {
    local CACHE="$HOME/.cache/nq-cache"
    if ! ( [ -e $CACHE ] && [ $(stat -c %Y $CACHE) -gt $(( $(date +%s) - 3600 )) ] ); then
        echo "update cache" && nix-env -qa --json > "$CACHE"
    fi
    jq -r 'to_entries | .[] | .key + "|" + .value.meta.description' < "$CACHE" |
        {
            if [ $# -gt 0 ]; then
                # double grep because coloring breaks column's char count
                # $* so that we include spaces (could do .* instead?)
                grep -i "$*" | column -t -s "|" | grep --color=always -i "$*"
            else
                column -t -s "|"
            fi
        }
}

setgitremote() {
    # I found myself doing this too often.
    local remoteUrl="$(git remote -v | grep -oP "http[^ ]+" | head -1)"
    local domain="$(echo $remoteUrl | cut -f3 -d'/')"
    local username="$(echo $remoteUrl | cut -f4 -d'/')"
    local reponame="$(echo $remoteUrl | cut -f5 -d'/' | cut -f1 -d'.' )"
    local newRemote="git@$domain:$username/$reponame.git"
    echo Setting git remote to $newRemote
    git remote set-url origin $newRemote
}

dec2hex() {
    printf "%X\n" $1
}

hex2dec() {
    printf "%d\n" 0x$1
}

extract() {      # Handy Extract Program
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

# if we're in st TERM, assume we're in xst and set to esc code, else default.
# dirty, but AFAIK there is no way to detect if esc code yields nothing visible from
# the shell (as it will see the escape code only either way).
case $TERM in
    st*)
        [ -z "$TMUX" ] && _prompt=$'\e[z'
        ;;
esac

prompt () {
    _ERR=$?
    _prompt="${_prompt:->}"
    [ $(jobs | wc -l) -ne 0 ] && _prompt="$_prompt$_prompt"
    [ $_ERR -ne 0 ] && _prompt="\e[7m$_prompt\e[0m" # invert
    echo -n -e "$_prompt "
}
# }}}

# {{{ shell
cur_shell=$(ps | grep $$ |  sed 's/^.* //')
history_length=10000
history_file="$HOME/.${cur_shell}_history"

case $cur_shell in
    bash)
        set_history() {
            HISTFILE="$history_file"
            HISTFILESIZE="$history_length"
            HISTSIZE="$history_length"

            shopt -s histappend
            HISTCONTROL=ignoredups:erasedups
        }
        ;;
    zsh)
        set_history() {
            HISTFILE="$HOME/.${cur_shell}_history"
            SAVEHIST="$history_length"
            HISTSIZE="$history_length"

            setopt hist_ignore_dups
            setopt share_history
            setopt hist_ignore_all_dups
            setopt hist_ignore_space
        }
        ;;
esac
# }}}

# autostartx if running on the first tty:
#[[ -z $DISPLAY && $XDG_VTNR -eq 1 && -z $TMUX ]] && exec startx
