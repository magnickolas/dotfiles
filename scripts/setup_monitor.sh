#!/bin/bash
USER=magnickolas
export DISPLAY=:0
export XAUTHORITY=/home/$USER/.Xauthority

function connect() {
    xrandr --output HDMI-2 --auto --left-of eDP-1
    xrandr --output eDP-1 --off
}

function disconnect() {
    xrandr --output eDP-1 --auto
    xrandr --output HDMI-2 --off
}
   
xrandr | grep "HDMI-2 connected" &> /dev/null && connect || disconnect
