# If not running interactively, don't do anything
[[ $- != *i* ]] && return
. $HOME/.profile

# get the thing
[ ! -d ~/.zplug ] && curl --create-dirs -o ~/.zplug/zplug https://raw.githubusercontent.com/b4b4r07/zplug/master/zplug
. ~/.zplug/zplug

# config the thing
zplug "plugins/git", from:oh-my-zsh                                  # git aliases
zplug "plugins/colored-man-pages", from:oh-my-zsh                    # distinct man page colors
zplug "zsh-users/zsh-autosuggestions", of:"zsh-autosuggestions.zsh"  # fish-like suggestions
zplug "supercrabtree/k"                                              # ls-like with niceties
zplug "djui/alias-tips", nice: 11                                    # alias reminder

# load the thing
! zplug check && zplug install
zplug load

PROMPT='$(prompt) '

[ -f ~/.fzf.zsh ] && . ~/.fzf.zsh || true
