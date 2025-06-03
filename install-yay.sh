#!/usr/bin/env bash

set -xe

if command -v yay > /dev/null
then
  exit 0
fi

sudo pacman -S --noconfirm --needed git base-devel
[ -d ~/src/yay ] || git clone https://aur.archlinux.org/yay.git ~/src/yay
pushd ~/src/yay
makepkg -si --noconfirm
popd
