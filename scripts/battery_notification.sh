#!/bin/sh
set -x
export DISPLAY=:0

get_battery_level() {
    level=$(acpi -b | cut -d',' -f2 | xargs)
    level=${level%'%'}
    echo $level
}

is_charging() {
    status=$(acpi -b)
    [ "$status" = "${status%'Discharging'*}" ]
}

# Notify if the battery level is less than some value
# (default 10%)
BATTERY_THRESHOLD=${1:-15}
# Make some pause between notifications
# (default 20 mins)
NOTIFY_DELAY=${2:-1200}

CACHE_FILE="/tmp/tmp.battery_notify_timestamp"

if [ -e $CACHE_FILE ]; then
  latest_notify_time=$(cat $CACHE_FILE)
fi

cur_battery_level=$(get_battery_level)
timestamp=$(date +'%s')

echo $cur_battery_level
echo $latest_notify_time

if (! is_charging) && [ \
     $cur_battery_level -lt $BATTERY_THRESHOLD -a \
       \( -z "$latest_notify_time" -o \
          $timestamp -gt $((latest_notify_time+NOTIFY_DELAY)) \
       \) \
   ]; then
    notify-send -u critical "Battery is low"
    echo $timestamp > $CACHE_FILE
else
    if [ -z $latest_notify_time ]; then
      latest_notify_time=$timestamp
    fi
    echo $latest_notify_time > $CACHE_FILE
fi
