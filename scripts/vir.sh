#!/bin/sh
set -e

monitor_exists() {
	xrandr --listactivemonitors | grep -q "$1 "
}

layout=$1
display="eDP-1"
if ! monitor_exists ${display}; then
	display="eDP-1-1"
fi
if ! monitor_exists ${display}; then
	printf "Error: %s does not exist" "${display}"
fi
subdisplay_1="${display}-F"
subdisplay_2="${display}-S"
# exit if layout is neither horizontal nor vertical
if [ "$layout" != "h" ] && [ "$layout" != "v" ]; then
	exit 1
fi
if ! monitor_exists "${subdisplay_1}"; then
	if [ "$layout" = "h" ]; then
		xrandr --setmonitor "${subdisplay_1}" 1920/344x540/97+0+0 "${display}"
		xrandr --setmonitor "${subdisplay_2}" 1920/344x540/97+0+540 none
	else
		xrandr --setmonitor "${subdisplay_1}" 960/172x1080/194+0+0 "${display}"
		xrandr --setmonitor "${subdisplay_2}" 960/172x1080/194+960+0 none
	fi
else
	xrandr --delmonitor "${subdisplay_1}"
	xrandr --delmonitor "${subdisplay_2}"
fi
