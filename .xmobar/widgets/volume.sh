#!/bin/bash
volume=$(awk -F'[][]' '/Left:/{ print $2 }' <(amixer sget Master))
volume=${volume%%%}
active=$(awk -F'[][]' '/Left:/{ print $4 }' <(amixer sget Master))

muted_symbol="<fc=#606060>婢</fc>"
volume_symbols=("奄" "奔" "墳")

status=${muted_symbol}

if [ "$active" == "on" ]; then
	if [ "$volume" -gt 66 ]; then
		status=${volume_symbols[2]}
	elif [ "$volume" -gt 33 ]; then
		status=${volume_symbols[1]}
	else
		status=${volume_symbols[0]}
	fi
fi

printf '%s%s%s%s%s%s%s' \
	'<action=`xdotool key XF86AudioMute`>' \
	'<action=`xdotool key XF86AudioRaiseVolume` button=4>' \
	'<action=`xdotool key XF86AudioLowerVolume` button=5>' \
	'<fn=4>' "${status}" '</fn>' \
	'</action></action></action>'
