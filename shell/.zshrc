# get the thing
[ ! -d ~/.zplug ] && curl --create-dirs -o ~/.zplug/zplug https://raw.githubusercontent.com/b4b4r07/zplug/master/zplug
. ~/.zplug/zplug

# config the thing
zplug "plugins/git", from:oh-my-zsh                                  # git aliases
zplug "plugins/colored-man-pages", from:oh-my-zsh                    # distinct man page colors
zplug "zsh-users/zsh-autosuggestions", of:"zsh-autosuggestions.zsh"  # fish-like suggestions
zplug "supercrabtree/k"                                              # ls-like with niceties
zplug "zsh-users/zsh-syntax-highlighting", nice:10                   # syntax highlighting
zplug "djui/alias-tips", nice: 11                                    # alias reminder

# load the thing
! zplug check && zplug install
zplug load

. $HOME/.profile
PROMPT='$(prompt) '

[ -f ~/.fzf.$0 ] && source ~/.fzf.$0 || true
