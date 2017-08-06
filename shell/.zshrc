# If not running interactively, don't do anything
[[ $- != *i* ]] && return
. $HOME/.profile

# get the thing
zplug_install=
if [ ! -d ~/.zplug ]; then
    curl -sL zplug.sh/installer | zsh
    zplug_install="zplug install"
fi

# interactive value when playing with plugins:
#zplug_install="! zplug check && zplug install"

. ~/.zplug/init.zsh

# config the thing
zplug "plugins/git", from:oh-my-zsh                                  # git aliases
zplug "zsh-users/zsh-autosuggestions", use:"zsh-autosuggestions.zsh" # fish-like suggestions
zplug "djui/alias-tips"                                              # alias reminder
zplug "junegunn/fzf", use:"shell/completion.zsh"                     # fzf
zplug "junegunn/fzf", use:"shell/key-bindings.zsh"                   # fzf

eval $zplug_install

# load the thing
zplug load

setopt PROMPT_SUBST

# allow emacs like bindings to work
bindkey -e
