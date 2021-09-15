#!/bin/sh
status=$(acpi -b)
if [ "$status" != "${status%"Discharging"*}" ]; then
    echo ''
else
    echo '⚡'
fi
