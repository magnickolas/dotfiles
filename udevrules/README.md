Put inside `/etc/udev/rules.d`, then either reboot or run
```console
udevadm control --reload-rules && udevadm trigger
```
