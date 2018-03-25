#!/bin/bash

connecteds=($(xrandr | grep " connected" | sed -e "s/\([A-Z0-9]\+\) connected.*/\1/"))
let len=${#connecteds[@]}-1
xrandr --output ${connecteds[0]} --auto
for i in $(seq 1 $len)
do
    let prev=i-1
    xrandr --output ${connecteds[$i]} --auto --right-of ${connecteds[prev]}
done
