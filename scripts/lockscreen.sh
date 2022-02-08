#!/bin/bash
wallpaper_path=${1:-$HOME/.wallpapers/wallpaper.jpg}
lock_path=${2:-$HOME/.wallpapers/lock.png}
lockscreen_prefix=$(dirname "${wallpaper_path}")/lockscreen
lockscreen_ext=png

display_resolution=$(xrandr --current | grep "\*" | uniq | awk '{print $1}')

resized_lockscreen_path="${lockscreen_prefix}_${display_resolution}.${lockscreen_ext}"

[[ -f "${resized_lockscreen_path}" ]] ||
	convert "${wallpaper_path}" -resize "${display_resolution}" -font Liberation-Sans \
		-pointsize 26 -fill white -gravity center \
		-annotate +0+110 "Type Password to Unlock" "${lock_path}" \
		-gravity center -composite "${resized_lockscreen_path}"

i3lock -i "${resized_lockscreen_path}"
