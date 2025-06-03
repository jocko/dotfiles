#!/usr/bin/env bash

set -xe

sudo pacman -S --noconfirm --needed gvim
ln -sf ~/.dotfiles/vimrc ~/.vimrc
[ -d ~/.vim/bundle/Vundle.vim ] || git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall
ln -sf --no-dereference ~/.dotfiles/vim/after ~/.vim/after
