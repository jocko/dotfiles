#!/usr/bin/env bash

sudo pacman -S --noconfirm --needed ttf-hack alacritty
ln -sf ~/.dotfiles/alacritty.toml ~/.alacritty.toml

sudo pacman -S --noconfirm --needed ttf-hack kitty
mkdir -p ~/.config/kitty
ln -sf ~/.dotfiles/current-theme.conf ~/.config/kitty/current-theme.conf
ln -sf ~/.dotfiles/kitty.conf ~/.config/kitty/kitty.conf

