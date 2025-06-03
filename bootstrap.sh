#!/usr/bin/env bash

set -xe

sudo pacman -Syu --noconfirm

./install-vim.sh

ln -sf ~/.dotfiles/bashrc ~/.bashrc
ln -sf ~/.dotfiles/inputrc ~/.inputrc
mkdir -p ~/bin ~/repos ~/src

sudo pacman -S --noconfirm --needed bash-completion wget tree atool screen pkgfile openssh man-db

./install-desktop.sh

sudo pacman -S --noconfirm --needed ttf-hack alacritty
ln -sf ~/.dotfiles/alacritty.toml ~/.alacritty.toml

sudo pacman -S --noconfirm --needed firefox

ln -sf ~/.dotfiles/dircolors ~/.dircolors
ln -sf ~/.dotfiles/screenrc ~/.screenrc

sudo pkgfile --update
sudo systemctl enable pkgfile-update.timer

./install-yay.sh
