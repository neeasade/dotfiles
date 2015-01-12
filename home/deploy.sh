#!/bin/bash

for i in $(ls); do
    stow -t /home/$USER/ $i;
done
