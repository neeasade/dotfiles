# If not running interactively, don't do anything
[[ $- != *i* ]] && return

. $HOME/.profile

# get the thing
[ ! -d ~/.zplug ] && curl -sL get.zplug.sh | zsh
. ~/.zplug/init.zsh

# config the thing
zplug "plugins/git", from:oh-my-zsh, nice:10                         # git aliases
zplug "plugins/colored-man-pages", from:oh-my-zsh                    # distinct man page colors
zplug "zsh-users/zsh-autosuggestions", use:"zsh-autosuggestions.zsh" # fish-like suggestions
zplug "supercrabtree/k"                                              # ls-like with niceties
zplug "djui/alias-tips"                                              # alias reminder

# load the thing
! zplug check && zplug install
zplug load

setopt PROMPT_SUBST
PS1='$(prompt) '

[ -f ~/.fzf.zsh ] && . ~/.fzf.zsh || true
