#!/usr/bin/env bash

ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"

brew install git
brew install vim --with-lua
brew install zsh
brew install tmux

brew install readline
brew install ruby-build
brew install rbenv-binstubs

brew tap thoughtbot/formulae
brew install rcm

brew install ctags
brew install ack
brew install wget
brew install tree

brew install caskroom/cask/brew-cask

git clone git@github.com:jocko/dotfiles.git "#{HOME}/.dotfiles"
rcup rcrc
rcup
 
brew update
brew doctor
