#!/bin/bash
USER=magnickolas
export DISPLAY=:0
export XAUTHORITY=/home/$USER/.Xauthority

xset r rate 220 55
setxkbmap -layout us,ru -option 'grp:alt_space_toggle,compose:ralt'
