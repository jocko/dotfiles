# My dotfiles

Tested on Ubuntu 22.04

## TODO

 * Clean up dunst config. Also, it doesn't need its own directory
 * Clean up i3blocks & i3status config
 * i3 doesn't need its own config directory
 * Make `mute` & `unmute` great again
 * See if I can find a sweet setup for jupyter-vim (vim-slime maybe?)
 * Rename repo to just `dotfiles`

## Bootstrap

Make sure `apt` is up to date and that `git` is installed.

If necessary, generate a new SSH key:

    ssh-keygen -t ed25519 -C "<email>"

Clone the dotfiles repo:

    git clone git@github.com:jocko/dotfiles-v2.git ~/.dotfiles

Link the git config:

    ln -sf ~/.dotfiles/gitconfig ~/.gitconfig
    ln -sf ~/.dotfiles/gitignore ~/.gitignore

Note that `gitconfig` doesn't specify `user.email`. Instead, set this
system wide:

    sudo git config --system user.email "<email>"

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

Start `vim` and run `:PluginInstall`.

Now, rest of the commands can be executed via slime from inside
vim. Launch a `:terminal` and do `<C-c><C-c>` below:

    sudo pwd

Install font of choice:

    sudo apt install -y fonts-hack-ttf

Create ~/bin:

    mkdir ~/bin

Install essential packages:

    sudo apt install -y build-essential curl direnv

Optionally, copy skeleton file(s):

    cp ~/.dotfiles/skel/bash_local ~/.bash_local

Optionally, symlink dircolors:

    ln -sf ~/.dotfiles/dircolors ~/.dircolors

## Terminal Emulator

Install `xfce4-terminal` and make it the default:

    sudo apt install -y xfce4-terminal \
        && sudo update-alternatives --config x-terminal-emulator

Install color scheme:

    cp ~/.dotfiles/skel/xfce4-gruvbox-dark.theme \
        ~/.local/share/xfce4/terminal/colorschemes/gruvbox-dark.theme

In `xfce4-terminal`, right click and uncheck `Show Menubar`. Right click
again and open `Preferences...`. Under the `Appearance` tab, set font to
`Hack Regular 14`. Under the `Colors` tab, load preset `gruvbox dark`.

## Window Manager

Official Ubuntu i3 packages might be quite old, install using i3's
own repository.

Download keyring:

    curl --silent https://debian.sur5r.net/i3/pool/main/s/sur5r-keyring/ \
        | perl -lne 'print "curl -o sur5r-keyring.deb https://debian.sur5r.net/i3/pool/main/s/sur5r-keyring/$1" if />(sur5r-keyring_.*\.deb)/' \
        | bash

Install it:

    sudo dpkg -i sur5r-keyring.deb && rm sur5r-keyring.deb

Configure repo:

    echo "deb [arch=amd64] http://debian.sur5r.net/i3/ $(lsb_release -cs) universe" \
        | sudo tee /etc/apt/sources.list.d/sur5r-i3.list

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

TODO Install `dunst`

Link config (note: probably need some love):

    ln -sf ~/.dotfiles/config/dunst ~/.config/dunst

Try it out:

    ding Ermahgerd!

## Firefox

This installs a non-snap version of Firefox.

Add PPA:

    sudo add-apt-repository ppa:mozillateam/ppa

Pin it:

    echo -e "Package: *\nPin: release o=LP-PPA-mozillateam\nPin-Priority: 1001" \
        | sudo tee /etc/apt/preferences.d/mozilla-firefox

Install `firefox`:

    sudo apt install -y firefox

## Python

Install `python3` and make `python` point to it:

    sudo apt install -y python3-pip python3-venv python-is-python3

## Ruby

Download `ruby-install` (note: version is static):

    wget -O ~/src/ruby-install.tar.gz https://github.com/postmodern/ruby-install/archive/v0.8.5.tar.gz \
        && rm -rf ~/src/ruby-install \
        && mkdir ~/src/ruby-install \
        && tar -xzvf ~/src/ruby-install.tar.gz --strip-components=1 -C ~/src/ruby-install \
        && rm ~/src/ruby-install.tar.gz

Install it:

    pushd ~/src/ruby-install/ \
        && sudo make install \
        && popd

Download latest ruby versions:

    ruby-install --update

Install ruby 3.1 (note: might take a while):

    ruby-install ruby 3.1

Download `chruby` (note: version is static):

    wget -O ~/src/chruby.tar.gz https://github.com/postmodern/chruby/archive/v0.3.9.tar.gz \
        && rm -rf ~/src/chruby \
        && mkdir ~/src/chruby \
        && tar -xzvf ~/src/chruby.tar.gz --strip-components=1 -C ~/src/chruby \
        && rm ~/src/chruby.tar.gz

Install it:

    pushd ~/src/chruby/ \
        && sudo make install \
        && popd

## Extras

    sudo apt install -y httpie

    sudo apt install -y jq

    sudo apt install -y fzf

    sudo apt install -y scrot

    sudo apt install -y pcmanfm \
        && xdg-mime default pcmanfm.desktop inode/directory

    sudo apt install -y sxiv \
        && xdg-mime default sxiv.desktop image/gif \
        && xdg-mime default sxiv.desktop image/jpeg \
        && xdg-mime default sxiv.desktop image/jpg \
        && xdg-mime default sxiv.desktop image/png

    sudo apt install -y ksnip

    sudo apt install -y atool

    sudo apt install -y gron

    sudo apt install -y tree

    # SDKMAN (without touching my dotfiles)

    curl -s "https://get.sdkman.io?rcupdate=false" | bash

    sdk install java

    sdk install maven

    # NVM (without touching my dotfiles)

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.2/install.sh | PROFILE=/dev/null bash

    nvm install 16

## Hacks

### Map caps-lock to escape

On Debian/Ubuntu (not Gnome!):

TODO Could this go in i3/config, just as I do for xset r rate?
```
# ~/.xsessionrc
setxkbmap -option caps:escape
```

For Gnome:

```
dconf write "/org/gnome/desktop/input-sources/xkb-options" "['caps:swapescape']"
```

### TODO touchpad point to click
