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
sudo apt install build-essential atool curl fonts-hack-ttf httpie jq gron fzf pcmanfm sxiv zathura direnv
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

TODO Make this work
-> % echo "deb http://debian.sur5r.net/i3/ $(grep '^DISTRIB_CODENAME=' /etc/lsb-release | cut -f2 -d=) universe" >> /etc/apt/sources.list.d/sur5r-i3.list
This is how it works for docker
      echo \
        "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
        $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

```
# /etc/apt/sources.list.d/sur5r-i3.list

deb [arch=amd64] http://debian.sur5r.net/i3/ focal universe
```

```
sudo apt install i3
ln -sf ~/.dotfiles/i3/config ~/.config/i3/config 
```

### xfce4-terminal

```
sudo apt install xfce4-terminal
sudo update-alternatives --config x-terminal-emulator
```

#### themes

https://github.com/arcticicestudio/nord-xfce-terminal

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

## Map caps-lock to escape

On Debian/Ubuntu (not Gnome!):

```
# ~/.xsessionrc

setxkbmap -option caps:escape
```

For Gnome:

```
dconf write "/org/gnome/desktop/input-sources/xkb-options" "['caps:swapescape']"
```

## sdkman

```
curl -s "https://get.sdkman.io" | bash
```

```
sdk list java
sdk list maven
```

## jupyter-vim (TODO)

### .vimrc

Plugin 'jupyter-vim/jupyter-vim'

let g:my_venv = fnamemodify('~/.vim/venv', ':p')
if exists('g:my_venv')
    pythonx import os; import vim
    pythonx activate_this = os.path.join(vim.eval('g:my_venv'), 'bin/activate_this.py')
    pythonx with open(activate_this) as f: exec(f.read(), {'__file__': activate_this})
endif

### qtconsole

pip3 install PyQt5 jupyter-qtconsole pandas matplotlib

jupyter qtconsole --generate-config

### ~/.jupyter/jupyter_console_config.py

c.ZMQTerminalInteractiveShell.include_other_output = True

## Move back to Bash (Experimental)

* Use vi mode
* echo 'set completion-ignore-case On' >> ~/.inputrc
* Need to fix my prompt (PS1='\[\e[0m\][\[\e[0m\]\w\[\e[0m\]]\n\[\e[0m\]-\[\e[0m\]> \[\e[0m\]\$ \[\e[0m\]')
