- `alacritty_scheme_switcher`
  Statically linked binary for switching [alacritty terminal](https://github.com/alacritty/alacritty) color schemes taken from https://github.com/magnickolas/alacritty-scheme-switcher.
- `battery_notification.sh` 
  Send notification on low battery charge if it's in discharging state. (executed by crontab every minute).
- `change_brightness.sh inc|dec` 
  Script to decrease/increase brightness. Specifically, do it smoothly when the brightness value is low.
- `setup_keyboard.sh`
  Set comfortable delay and repeat rate for keyboard (executed by i3wm on startup and by udev rule triggered on connection of USB keyboard).
- `setup_monitor.sh`
  Script for laptop to keep an external monitor as the only screen and switch things back if it's disconnected (executed by udev rule triggered on connection/disconnection of external monitor). Displays names are hardcoded for my laptop.
