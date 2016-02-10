#!/bin/bash
# Deploy these dotfiles to your setup.

cd $(dirname $0)/..

# if run with no arguments, setup packages/submodules.
if [ -z "$@" ]; then
    git submodule init
    git submodule update

    for package in $(cat meta/depends.txt); do
        yaourt -S $package --needed --noconfirm
    done
fi

echo "backing up any conflicts to ~/dotfile_conflicts.."
IFS=$'\n'
for file in $(stow -n */ 2>&1 | grep -oE ":.+" | cut -c3-); do
    mkdir -p ~/dotfile_conflicts/$(dirname $file)
    mv ~/$file ~/dotfile_conflicts/$file
    echo $file
done

echo "Linking dotfiles to home dir.."
stow */

$HOME/.wm/scripts/ltheme twilight
echo "All done."
