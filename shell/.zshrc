# If not running interactively, don't do anything
[[ $- != *i* ]] && return
. $HOME/.profile

# get the thing
[ ! -d ~/.zplug ] && curl -sL zplug.sh/installer | zsh
. ~/.zplug/init.zsh

# config the thing
zplug "plugins/git", from:oh-my-zsh, defer:0                         # git aliases
zplug "zsh-users/zsh-autosuggestions", use:"zsh-autosuggestions.zsh" # fish-like suggestions
zplug "djui/alias-tips"                                              # alias reminder
zplug "junegunn/fzf", use:"shell/completion.zsh"                     # fzf
zplug "junegunn/fzf", use:"shell/key-bindings.zsh"                   # fzf

# load the thing
! zplug check && zplug install
zplug load

setopt PROMPT_SUBST

# allow emacs like bindings to work
bindkey -e
