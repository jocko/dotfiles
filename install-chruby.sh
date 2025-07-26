#!/usr/bin/env bash

ln -sf ~/.dotfiles/gemrc ~/.gemrc

wget -O ~/src/ruby-install.tar.gz https://github.com/postmodern/ruby-install/archive/v0.10.1.tar.gz
rm -rf ~/src/ruby-install
mkdir ~/src/ruby-install
tar -xzvf ~/src/ruby-install.tar.gz --strip-components=1 -C ~/src/ruby-install
rm ~/src/ruby-install.tar.gz

cd ~/src/ruby-install/
sudo make install

wget -O ~/src/chruby.tar.gz https://github.com/postmodern/chruby/archive/v0.3.9.tar.gz
rm -rf ~/src/chruby
mkdir ~/src/chruby
tar -xzvf ~/src/chruby.tar.gz --strip-components=1 -C ~/src/chruby
rm ~/src/chruby.tar.gz

cd ~/src/chruby/
sudo make install
