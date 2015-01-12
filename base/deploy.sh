#!/bin/bash

for i in $(ls); do
    stow -t / $i;
done
