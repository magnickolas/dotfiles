#!/bin/bash
set -ex
adb reverse tcp:5900 tcp:5900
width=${1:-1280}
height=${2:-720}
fps=${3:-60}
mode="${width}x${height}_${fps}.00"
primary_display="eDP-1"
virt_display="HDMI-1"
xrandr --newmode $(gtf "${width}" "${height}" "${fps}" | sed -ne 's/"//g;s/ Modeline //p') || true
xrandr --output "${virt_display}" --off
xrandr --addmode "${virt_display}" "${mode}"
xrandr --output "${virt_display}" --mode "${mode}" --below "${primary_display}"
x11vnc -noskip_dups -repeat -rfbauth "${HOME}"/.vnc/passwd -clip "${width}"x"${height}"+0+1080 -localhost
