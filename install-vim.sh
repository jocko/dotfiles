#!/usr/bin/env bash

set -xe

command -v vim > /dev/null || sudo pacman -S --noconfirm gvim
ln -sf ~/.dotfiles/vimrc ~/.vimrc
[ -d ~/.vim/bundle/Vundle.vim ] || git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall
ln -sf ~/.dotfiles/vim/after ~/.vim/after
