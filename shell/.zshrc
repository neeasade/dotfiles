. $HOME/.profile

setopt PROMPT_SUBST

# completion like emacs, use cache
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*' use-cache true

# completion after '='(like --prefix=/usr)
setopt magic_equal_subst

# case insensitive path completion
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# if the entry is not a command but matches dir, cd to that dir.
setopt autocd

# Remove '/' and '-' from $WORDCHARS for finer Ctrl-w behaviour
export WORDCHARS='*?_.[]~=&;!#$%^(){}<>'

setopt autopushd

if $ESHELL; then
    unsetopt PROMPT_SP
    unsetopt zle
    return
fi

# vim bindings

bindkey -v

zle-keymap-select () {
    if [ $KEYMAP = vicmd ]; then
        cursorStyle block
    else
        cursorStyle bar
    fi
    zle reset-prompt
    zle -R
}
zle -N zle-keymap-select

zle-line-init () {
    zle -K viins
    cursorStyle bar
}
zle -N zle-line-init

# text object extension -- eg ci" da(:
autoload -U select-quoted
zle -N select-quoted
for m in visual viopp; do
    for c in {a,i}{\',\",\`}; do
        bindkey -M $m $c select-quoted
    done
done

# match escape to evil
bindkey tn vi-cmd-mode
bindkey -M visual tn vi-cmd-mode
bindkey -M viopp tn vi-cmd-mode

# match some standard readline binds in insert mode
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line

# TODO: map this to fzf (plugin above is bindkey -e setup)
bindkey '^r' history-incremental-search-backward
