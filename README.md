## git

```
sudo apt install git
```

```
git clone git@github.com:jocko/dotfiles-v2.git ~/.dotfiles
ln -sf ~/.dotfiles/gitconfig ~/.gitconfig
ln -sf ~/.dotfiles/gitignore ~/.gitignore
```

## vim

```
sudo apt install vim
ln -sf ~/.dotfiles/vimrc ~/.vimrc
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
mkdir ~/.vim/swap
```

## misc

```
sudo apt install build-essential atool curl fonts-hack-ttf httpie jq gron fzf pcmanfm sxiv zathura
mkdir ~/bin
```

## i3

Download keyring (apt-helper download-file ...)
https://debian.sur5r.net/i3/pool/main/s/sur5r-keyring/
```
sudo dpkg -i ./keyring.deb
```

```
lsb_release -c
```

```
# /etc/apt/sources.list.d/sur5r-i3.list

deb [arch=amd64] http://debian.sur5r.net/i3/ focal universe
```

```
sudo apt install i3
ln -sf ~/.dotfiles/i3/config ~/.config/i3/config 
```


### rofi

```
sudo apt install rofi
```

### i3blocks

```
sudo apt install i3blocks
ln -sf ~/.dotfiles/i3blocks.conf ~/.i3blocks.conf
```

### xkblayout-state


```
git clone git@github.com:nonpop/xkblayout-state.git ~/src/xkblayout-state
sudo apt install libx11-dev
make
cp xkblayout-state ~/bin
```

### dunst

TODO This config need some love. Not using at the moment.

```
ln -sf ~/.dotfiles/config/dunst ~/.config/dunst
```

### xdg-mime

```
xdg-mime query default inode/directory
xdg-mime default pcmanfm.desktop inode/directory

xdg-mime query default application/pdf
xdg-mime default org.pwmt.zathura.desktop application/pdf
```

## zsh

```
sudo apt install zsh
chsh -s $(which zsh)
ln -sf ~/.dotfiles/zshrc ~/.zshrc
```

## python

```
sudo apt install python3-pip python3-venv
sudo apt install build-essential libssl-dev libffi-dev python3-dev libxml2-dev libxslt-dev
```

## sdkman

```
curl -s "https://get.sdkman.io" | bash
```

```
sdk list java
sdk list maven
```
