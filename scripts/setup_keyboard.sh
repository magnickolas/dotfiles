#!/bin/sh
USER=magnickolas
sudo -u $USER sh -c "export DISPLAY=:0; xset r rate 220 55"
sudo -u $USER sh -c "export DISPLAY=:0; sleep 0.2; setxkbmap -layout us,ru -option 'grp:alt_space_toggle,compose:ralt'"
