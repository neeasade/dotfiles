#!/bin/bash

for i in $(ls -d */); do
    stow -t /home/$USER/ $i;
done
