#!/bin/sh
set -ex
HOME=~magnickolas
export DISPLAY=:0
export XAUTHORITY="$HOME"/.Xauthority
display_list=$(xrandr -q | grep '\bconnected\b' | cut -d' ' -f1)' '
first_display=$(echo "${display_list}" | sed -n 1p | xargs)
second_display=$(echo "${display_list}" | sed -n 2p | xargs)

turn_to_second() {
	xrandr --output "${first_display}" --off
	xrandr --output "${second_display}" --auto
}

connect_first() {
	xrandr --output "${second_display}" --off
	xrandr --output "${first_display}" --auto
}

if [ -n "${second_display}" ]; then
	turn_to_second
else
	connect_first
fi

"$HOME"/scripts/set_wallpaper.sh
