#!/bin/sh

git submodule init

# Install the depends.txt programs using yaourt:

for i in $(ls -d */ | grep -v "conflict"); do
    cd $i
        echo "Installing from ${i}depends.txt:"
        for package in $(cat ./depends.txt); do
            yaourt -S $package --needed
        done
    cd ..
done

