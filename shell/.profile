# if not running interactively, bail
test "$- != *i*" || exit

# source everything
for file in $HOME/.sh.d/*; do
    . $file
done

defaulttitle
