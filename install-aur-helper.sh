#!/usr/bin/env bash

set -xe

sudo pacman -S --noconfirm --needed base-devel

if ! command -v yay > /dev/null
then
  [ -d ~/src/yay ] || git clone https://aur.archlinux.org/yay.git ~/src/yay
  cd ~/src/yay
  makepkg -si --noconfirm
fi

# TODO configure --bottomup
if ! command -v paru > /dev/null
then
  [ -d ~/src/paru ] || git clone https://aur.archlinux.org/paru.git ~/src/paru
  cd ~/src/paru
  makepkg -si --noconfirm
fi
mkdir -p ~/.config/paru
ln -sf ~/.dotfiles/paru.conf ~/.config/paru/paru.conf
