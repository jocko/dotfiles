#!/usr/bin/env bash

set -xe

sudo pacman -S --noconfirm --needed sddm
sudo systemctl enable sddm
sudo mkdir -p /etc/sddm.conf.d
sudo cp -u sddm-theme.conf /etc/sddm.conf.d/theme.conf

sudo pacman -S --noconfirm --needed plasma-desktop dolphin kde-gtk-config \
  plasma-nm plasma-pa plasma-disks print-manager system-config-printer \
  spectacle okular kscreen ark kate

kwriteconfig6 --file kcminputrc --group Keyboard --key RepeatDelay 200
kwriteconfig6 --file kcminputrc --group Keyboard --key RepeatRate 30
kwriteconfig6 --file kxkbrc --group Layout --key Options caps:escape,grp:shifts_toggle
kwriteconfig6 --file kxkbrc --group Layout --key LayoutList us,se
kwriteconfig6 --file kxkbrc --group Layout --key Use true
kwriteconfig6 --file kxkbrc --group Layout --key ResetOldOptions true

lookandfeeltool -a org.kde.breezetwilight.desktop

# TODO Customize lock screen mod+esc
# TODO Map mod+d to krunner
