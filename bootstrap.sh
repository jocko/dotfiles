#!/usr/bin/env bash

set -xe

# sudo pacman -Syu --noconfirm

sudo pacman -S --noconfirm --needed bash-completion wget tree \
  atool screen pkgfile openssh man-db httpie xorg-xwayland firefox \
  bat vi ripgrep meld plocate jq brightnessctl unzip pavucontrol \
  direnv bc

ln -sf ~/.dotfiles/bashrc ~/.bashrc
mkdir -p ~/.bashrc.d
cp -u ~/.dotfiles/bashrc.d/* ~/.bashrc.d/
ln -sf ~/.dotfiles/inputrc ~/.inputrc
ln -sf ~/.dotfiles/dircolors ~/.dircolors
ln -sf ~/.dotfiles/screenrc ~/.screenrc
ln -sf ~/.dotfiles/gitconfig ~/.gitconfig
ln -sf ~/.dotfiles/gitignore ~/.gitignore
ln -sf ~/.dotfiles/irbrc ~/.irbrc
mkdir -p ~/bin ~/repos ~/src

./install-vim.sh
./install-ctags.sh

sudo pkgfile --update
sudo systemctl enable pkgfile-update.timer

sudo ln -sf /usr/share/man/man1/gawk.1.gz /usr/share/man/man1/awk.1.gz

sudo ex -sc '%s/^#Color/Color/ | x' /etc/pacman.conf

systemctl --user enable ssh-agent.service
