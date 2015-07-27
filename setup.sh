#!/bin/sh

git submodule init

# Install the depends.txt programs using yaourt:
for package in $(cat ./depends.txt); do
    yaourt -S $package --needed --noconfirm
done

