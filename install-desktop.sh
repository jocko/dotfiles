#!/usr/bin/env bash

set -xe

sudo pacman -S --noconfirm --needed sway xdg-desktop-portal-wlr wmenu otf-font-awesome \
  network-manager-applet
sudo pacman -S --noconfirm --needed pcmanfm gvfs
mkdir -p ~/.config/sway
ln -sf ~/.dotfiles/sway.config ~/.config/sway/config 

sudo pacman -S --noconfirm --needed waybar
mkdir -p ~/.config/waybar
ln -sf ~/.dotfiles/waybar.jsonc ~/.config/waybar/config.jsonc
ln -sf ~/.dotfiles/waybar.css ~/.config/waybar/style.css

sudo pacman -S --noconfirm --needed grim slurp swappy
mkdir -p ~/.config/swappy
ln -sf ~/.dotfiles/swappy.config ~/.config/swappy/config

sudo pacman -S --noconfirm --needed dunst
mkdir -p ~/.config/dunst
ln -sf ~/.dotfiles/dunstrc ~/.config/dunst/dunstrc

sudo cp -u ~/.dotfiles/profile.d/sway.sh /etc/profile.d/

