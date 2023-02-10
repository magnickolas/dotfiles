#!/bin/sh
cp --parents -r .zshrc .zshenv .config/zsh ~
cp --parents -r .xmonad ~
cp .wezterm.lua ~
cp -r .config ~
cp -r .local ~
cp -r .wallpapers ~
cp -r scripts ~
cp -r .fonts ~ && fc-cache -rvf
