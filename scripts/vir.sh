#!/bin/sh
set -e
layout=$1
# exit if layout is neither horizontal nor vertical
if [ "$layout" != "h" ] && [ "$layout" != "v" ]; then
	exit 1
fi
if ! xrandr --listactivemonitors | grep -q 'eDP-1-1'; then
	if [ "$layout" = "h" ]; then
		xrandr --setmonitor eDP-1-1 1920/344x540/97+0+0 eDP-1
		xrandr --setmonitor eDP-1-2 1920/344x540/97+0+540 none
	else
		xrandr --setmonitor eDP-1-1 960/172x1080/194+0+0 eDP-1
		xrandr --setmonitor eDP-1-2 960/172x1080/194+960+0 none
	fi
else
	xrandr --delmonitor eDP-1-1
	xrandr --delmonitor eDP-1-2
fi
