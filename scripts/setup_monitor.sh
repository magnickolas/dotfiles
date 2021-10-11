#!/bin/sh
HOME=$(echo ~magnickolas)
export DISPLAY=:0
export XAUTHORITY=$HOME/.Xauthority

connect() {
    xrandr --output HDMI-2 --auto --left-of eDP-1
    xrandr --output eDP-1 --off
}

disconnect() {
    xrandr --output eDP-1 --auto
    xrandr --output HDMI-2 --off
}
   
xrandr | grep "HDMI-2 connected" && connect || disconnect
