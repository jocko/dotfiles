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

When it comes to vim, there are a couple of options. Package `vim`
works okay, but doesn't have `xterm_clipboard` (can be checked in vim
by doing `:echo has('clipboard')`). Better option then is to install
`vim-gtk3`. Also, installing from PPA might give us a newer vim
version. For example:

    sudo add-apt-repository ppa:jonathonf/vim # Optional step

    sudo apt install -y vim-gtk3

Link the vim config and create swap directory:

    ln -sf ~/.dotfiles/vimrc ~/.vimrc && mkdir ~/.vim/swap

Install Vundle:

    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

Start `vim` and run `:PluginInstall`. Now, rest of the commands can be
executed from inside vim, using slime (`<C-c><C-c>` and `:terminal`).

Install font of choice:

    sudo apt install -y fonts-hack-ttf

Create ~/bin:

    mkdir ~/bin

Install essential packages:

    sudo apt install -y build-essential curl direnv

## Terminal emulator

Install `xfce4-terminal` and make it the default:

    sudo apt install -y xfce4-terminal

    sudo update-alternatives --config x-terminal-emulator

Install color scheme:

    ln -sf ~/.dotfiles/local/share/xfce4/terminal/colorschemes/gruvbox-dark.theme ~/.local/share/xfce4/terminal/colorschemes/

In `xfce4-terminal`, right click and uncheck `Show Menubar`. Right click
again and open `Preferences...`. Under the `Appearance` tab, set font to
`Hack Regular 14`. Under the `Colors` tab, load preset `gruvbox dark`.

## i3 Window Manager

Official Ubuntu i3 packages might be quite old, install using i3's
own repository.

Download keyring:

    curl --silent https://debian.sur5r.net/i3/pool/main/s/sur5r-keyring/ | perl -lne 'print "curl -o sur5r-keyring.deb https://debian.sur5r.net/i3/pool/main/s/sur5r-keyring/$1" if />(sur5r-keyring_.*\.deb)/' | bash

Install it:

    sudo dpkg -i sur5r-keyring.deb && rm sur5r-keyring.deb

Configure repo:

    echo "deb [arch=amd64] http://debian.sur5r.net/i3/ $(lsb_release -cs) universe" | sudo tee /etc/apt/sources.list.d/sur5r-i3.list

Update apt and install `i3`:

    sudo apt update && sudo apt install -y i3

Link config:

    ln -sf ~/.dotfiles/i3/config ~/.config/i3/config 

Install `rofi` (dmenu replacement)

    sudo apt install -y rofi

Install `xkblayout-state`

    sudo apt install -y libx11-dev

    mkdir -p ~/src && git clone git@github.com:nonpop/xkblayout-state.git ~/src/xkblayout-state

    pushd ~/src/xkblayout-state && make && cp xkblayout-state ~/bin && popd

Install `i3blocks`:

    sudo apt install -y i3blocks

Link config:

    ln -sf ~/.dotfiles/i3blocks.conf ~/.i3blocks.conf

## PCManFM

Install `pcmanfm`:

    sudo apt install -y pcmanfm

Configure mime type(s):

    xdg-mime default pcmanfm.desktop inode/directory

Inspect mime type(s):

    xdg-mime query default inode/directory

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


## Firefox

TODO Try out non-snap version (ppa:mozillateam/ppa) and document

## Extras

    sudo apt install httpie

    sudo apt install jq

    sudo apt install gron

    sudo apt install fzf

    sudo apt install atool

    curl -s "https://get.sdkman.io" | bash

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

