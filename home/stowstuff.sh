#!/bin/bash

read -p "Is this a single monitor setup?" -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
    for i in $(ls); do
       [[ $i != *"multi"* ]] && stow $i;
    done
else
    for i in $(ls); do
        stow $i;
    done
fi
