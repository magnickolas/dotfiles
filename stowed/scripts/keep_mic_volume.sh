#!/bin/sh
poll() {
    while read -r _; do
        pacmd set-source-volume \
            alsa_input.usb-Audio_Technica_Corp_ATR2500x-USB_Microphone-00.analog-stereo \
            65535
    done
}

pactl subscribe | poll
