#!/bin/sh
# Spaghetti code makes the world go round.

git submodule init

cd ./bin/bin/.colort
make

cd ../../..

cd ./bin/bin/.gtkreload
make

cd ../../..

# Install the depends.txt programs using yaourt:
for package in $(cat ./depends.txt); do
    yaourt -S $package --needed --noconfirm
done

