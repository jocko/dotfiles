#!/usr/bin/env bash

sudo pacman -S --noconfirm --needed ctags
mkdir -p ~/.ctags.d/
ln -sf ~/.dotfiles/typescript.ctags ~/.ctags.d/typescript.ctags

