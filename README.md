# My dotfiles

Tested on Ubuntu 22.04

## Bootstrap

Make sure apt is up to date and that `git` is installed.

Clone the dotfiles repo:

    git clone git@github.com:jocko/dotfiles-v2.git ~/.dotfiles

Link the git config:

    ln -sf ~/.dotfiles/gitconfig ~/.gitconfig
    ln -sf ~/.dotfiles/gitignore ~/.gitignore

Note that `gitconfig` doesn't specify `user.email`. Instead, set this
system wide:

    sudo git config --system user.email "..."

Link bash config

    link -sf ~/.dotfiles/bashrc ~/.bashrc

When it comes to vim, there are a couple of options. Package `vim` works
okay, but doesn't have `xterm_clipboard` (can be checked in vim by doing
`:echo has('clipboard')`). Better then to install `vim-gtk3`. Also,
installing from PPA might give us a newer vim version. For example:

    sudo add-apt-repository ppa:jonathonf/vim # Optional step
    sudo apt install vim-gtk3

Link the vim config and create swap directory:

    ln -sf ~/.dotfiles/vimrc ~/.vimrc
    mkdir ~/.vim/swap

Install Vundle:

    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

Start `vim` and run `:PluginInstall`.

Install font of choice:

    sudo apt install fonts-hack-ttf

Create ~/bin:

    mkdir ~/bin

Install essential packages:

    sudo apt install build-essential curl direnv

## Terminal emulator

Install `xfce4-terminal` and make it the default:

    sudo apt install xfce4-terminal
    sudo update-alternatives --config x-terminal-emulator

Install color scheme:

    ln -sf ~/.dotfiles/local/share/xfce4/terminal/colorschemes/gruvbox-dark.theme ~/.local/share/xfce4/terminal/colorschemes/

In `xfce4-terminal`, right click and uncheck `Show Menubar`. Right click
again and open `Preferences...`. Under the `Appearance` tab, set font to
`Hack Regular 14`. Under the `Colors` tab, load preset `gruvbox dark`.

## Firefox

TODO Try out non-snap version (ppa:mozillateam/ppa) and document

## i3 Window Manager

Official Ubuntu i3 packages might be quite old, install using i3's
own repository.

Download keyring:

    curl --silent https://debian.sur5r.net/i3/pool/main/s/sur5r-keyring/ | perl -lne 'print "curl -o sur5r-keyring.deb https://debian.sur5r.net/i3/pool/main/s/sur5r-keyring/$1" if />(sur5r-keyring_.*\.deb)/' | bash

Install it:

    sudo dpkg -i sur5r-keyring.deb

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

### misc

```
sudo apt install build-essential atool curl fonts-hack-ttf httpie jq gron fzf pcmanfm sxiv zathura direnv
mkdir ~/bin
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
sudo apt install python3-pip python3-venv python-is-python3
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

pip3 install PyQt5 jupyter pandas matplotlib

jupyter qtconsole --generate-config

### ~/.jupyter/jupyter_console_config.py

c.ZMQTerminalInteractiveShell.include_other_output = True

## Move back to Bash (Experimental)

* Use vi mode
* echo 'set completion-ignore-case On' >> ~/.inputrc
* Need to fix my prompt (PS1='\[\e[0m\][\[\e[0m\]\w\[\e[0m\]]\n\[\e[0m\]-\[\e[0m\]> \[\e[0m\]\$ \[\e[0m\]')
