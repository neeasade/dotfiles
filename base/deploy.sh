#!/bin/bash

for i in $(ls -d */); do
    stow -t / $i;
done
