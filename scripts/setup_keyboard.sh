#!/bin/sh
HOME=$(echo ~magnickolas)
export DISPLAY=:0
export XAUTHORITY=$HOME/.Xauthority

xset r rate 220 55
setxkbmap -layout us,ru -option 'grp:alt_space_toggle,compose:ralt'
