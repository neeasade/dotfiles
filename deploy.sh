#!/bin/bash
# Deploy these dotfiles to your setup.

cd $(dirname $0)

# only need to run setup.sh once.
if [ ! -e ./bin/bin/colort ]; then
    ./setup.sh
fi

mkdir -p ~/dotfile_conflicts
echo "backing up conflicts to ~/dotfile_conflicts.."
IFS=$'\n'
for file in $(stow -n $(ls */ -d | grep -v root) 2>&1 | grep -oE ":.+" | cut -c3-); do
    mkdir -p ~/dotfile_conflicts/$(dirname $file)
    mv ~/$file ~/dotfile_conflicts/$file
    echo $file
done

echo "Linking dotfiles to home dir.."
stow $(ls */ -d | grep -v root)

echo "All done, unless you want to also link the 'root' folder using 'stow root'"
