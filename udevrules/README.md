Put inside `/etc/udev/rules.d`, then either reboot or run
```console
sudo udevadm control --reload-rules && sudo udevadm trigger
```
