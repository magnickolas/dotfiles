#!/bin/bash
cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" || exit
git clone "https://github.com/xmonad/xmonad" xmonad-git
git clone "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
stack init
stack install dbus
stack install
sudo ln -s ~/.local/bin/xmonad /usr/local/bin
