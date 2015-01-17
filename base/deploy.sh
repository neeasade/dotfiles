#!/bin/bash

for i in $(ls -d */); do
    sudo stow -t / $i;
done
