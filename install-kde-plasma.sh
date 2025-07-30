#!/usr/bin/env bash

set -xe

sudo pacman -S --noconfirm --needed sddm
sudo systemctl enable sddm
sudo mkdir -p /etc/sddm.conf.d
sudo cp -u sddm-theme.conf /etc/sddm.conf.d/theme.conf

sudo pacman -S --noconfirm --needed plasma-desktop dolphin kde-gtk-config \
  plasma-nm plasma-pa plasma-disks print-manager system-config-printer \
  spectacle okular kscreen ark

kwriteconfig6 --file kcminputrc --group Keyboard --key RepeatDelay 200
kwriteconfig6 --file kcminputrc --group Keyboard --key RepeatRate 30
kwriteconfig6 --file kxkbrc --group Layout --key Options caps:escape

