#!/usr/bin/env bash

set -xe

sudo pacman -S --noconfirm --needed ttf-hack alacritty
ln -sf ~/.dotfiles/alacritty.toml ~/.alacritty.toml

sudo pacman -S --noconfirm --needed sway xdg-desktop-portal-wlr wmenu otf-font-awesome \
  network-manager-applet swaylock swayidle
sudo pacman -S --noconfirm --needed pcmanfm gvfs
mkdir -p ~/.config/sway
ln -sf ~/.dotfiles/sway.config ~/.config/sway/config 

sudo pacman -S --noconfirm --needed waybar
mkdir -p ~/.config/waybar
ln -sf ~/.dotfiles/waybar.jsonc ~/.config/waybar/config.jsonc
ln -sf ~/.dotfiles/waybar.css ~/.config/waybar/style.css

sudo pacman -S --noconfirm --needed grim slurp swappy wl-clipboard
mkdir -p ~/.config/swappy
ln -sf ~/.dotfiles/swappy.config ~/.config/swappy/config

sudo pacman -S --noconfirm --needed dunst
mkdir -p ~/.config/dunst
ln -sf ~/.dotfiles/dunstrc ~/.config/dunst/dunstrc

sudo pacman -S --noconfirm --needed swayimg
mkdir -p ~/.config/swayimg
ln -sf ~/.dotfiles/swayimg ~/.config/swayimg/config
