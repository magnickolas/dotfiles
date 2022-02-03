#!/bin/sh
WALLPAPER=${1:-wallpaper.jpg}
convert "${WALLPAPER}" -resize 1920x1080 -font Liberation-Sans \
    -pointsize 26 -fill white -gravity center \
    -annotate +0+110 "Type Password to Unlock" lock.png \
    -gravity center -composite lockscreen.png
