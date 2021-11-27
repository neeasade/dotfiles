# if not running interactively, bail
case "$-" in
    *i*) ;;
    *) return;;
esac

# source everything
for file in $HOME/.sh.d/*; do
    . $file
done

# host/local
if [ -f "$HOME/extend.sh" ]; then
    . "$HOME/extend.sh"
fi

defaulttitle
