#!/bin/bash
# Deploy these dotfiles to your setup.

cd $(dirname $0)

# if run with no arguments, setup packages/submodules/scripts.
if [ -z "$@" ]; then
    git submodule init

    cd ./bin/bin/.colort
    make
    cd ../.gtkreload
    make
    cd ../../..

    for package in $(cat ./depends.txt); do
        yaourt -S $package --needed --noconfirm
    done
fi

echo "backing up any conflicts to ~/dotfile_conflicts.."
IFS=$'\n'
for file in $(stow -n $(ls */ -d | grep -v root) 2>&1 | grep -oE ":.+" | cut -c3-); do
    mkdir -p ~/dotfile_conflicts/$(dirname $file)
    mv ~/$file ~/dotfile_conflicts/$file
    echo $file
done

echo "Linking dotfiles to home dir.."
stow $(ls */ -d | grep -v root)

echo "All done."
