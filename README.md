# My dotfiles

Tested on Ubuntu 22.04

## Bootstrap

Make sure `apt` is up to date and that `git` is installed.

If necessary, generate a new SSH key:

    ssh-keygen -t ed25519 -C "<email>"

Clone the dotfiles repo:

    git clone git@github.com:jocko/dotfiles.git ~/.dotfiles

Link bash and readline config:

    ln -sf ~/.dotfiles/bashrc ~/.bashrc && ln -sf ~/.dotfiles/inputrc ~/.inputrc

When it comes to vim, there are a couple of options. Package `vim`
works okay, but doesn't have `xterm_clipboard` (can be checked in vim
by doing `:echo has('clipboard')`). Better option then is to install
`vim-gtk3`. Also, installing from PPA might give us a newer vim
version. For example:

    sudo add-apt-repository ppa:jonathonf/vim # Optional step

    sudo apt install -y vim-gtk3

Link the vim config:

    ln -sf ~/.dotfiles/vimrc ~/.vimrc

Install Vundle:

    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

Start `vim` and run `:PluginInstall`.

Now, rest of the commands can be executed via slime from inside
vim. Launch a `:terminal` and do `<C-c><C-c>` below:

    sudo pwd

Link the git config:

    ln -sf ~/.dotfiles/gitconfig ~/.gitconfig \
        && ln -sf ~/.dotfiles/gitignore ~/.gitignore

Note that `gitconfig` doesn't specify `user.email`. Instead, set this
system wide:

    sudo git config --system user.email "<email>"

Link custom vim `after`:

    ln -sf ~/.dotfiles/vim/after ~/.vim/after

Create dirs in home:

    mkdir ~/bin ~/repos ~/src

Install essential packages:

    sudo apt install -y build-essential curl direnv wget

Optionally, copy skeleton file(s):

    cp ~/.dotfiles/skel/bash_local ~/.bash_local \
        && cat ~/.bash_local

Optionally, symlink dircolors:

    ln -sf ~/.dotfiles/dircolors ~/.dircolors

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

    mkdir -p ~/.config/i3 && ln -sf ~/.dotfiles/i3.config ~/.config/i3/config 

Install `rofi` (dmenu replacement)

    sudo apt install -y rofi

Install `xkblayout-state`

    sudo apt install -y libx11-dev

    git clone git@github.com:nonpop/xkblayout-state.git ~/src/xkblayout-state

    pushd ~/src/xkblayout-state && make && cp xkblayout-state ~/bin && popd

Install `i3blocks`:

    sudo apt install -y i3blocks

Link config:

    ln -sf ~/.dotfiles/i3blocks.conf ~/.i3blocks.conf

Link `dunst` config (`dunst` is recommended by `i3`, so it should already be
installed):

    mkdir -p ~/.config/dunst && ln -sf ~/.dotfiles/dunstrc ~/.config/dunst/dunstrc

Try it out:

    ln -sf ~/.dotfiles/scripts/ding ~/bin/ding && ~/bin/ding echo Ermahgerd!

Log out and then login to i3.

## Terminal Emulator

Install font of choice:

    sudo apt install -y fonts-hack-ttf

Choose either `xfce4-terminal` or `gnome-terminal`. They should work equally
well AFAIK.

### xfce4-terminal

Install `xfce4-terminal` and make it the default:

    sudo apt install -y xfce4-terminal \
        && sudo update-alternatives --config x-terminal-emulator

Install color scheme:

    mkdir -p ~/.local/share/xfce4/terminal/colorschemes \
        && cp ~/.dotfiles/skel/xfce4-gruvbox-dark.theme \
        ~/.local/share/xfce4/terminal/colorschemes/gruvbox-dark.theme

In `xfce4-terminal`, right click and open `Preferences...`. Under the `General`
tab. Set `Scrollbar is:` to `Disabled`. Uncheck `Show unsafe paste dialog`.
Under the `Appearance` tab, set font to `Hack Regular 14`. Uncheck `Display
menubar in new windows`. Under the `Colors` tab, load preset `gruvbox dark`.

### GNOME Terminal

On a fresh Ubuntu install, `gnome-terminal` is the default. If not, configure it:

    sudo update-alternatives --config x-terminal-emulator

The Gogh install script will fail on a fresh install because it assumes some
config is already in place. To work around this, manually create a dummy
profile via preferences. 

Then, clone Gogh repo and install color scheme:

    git clone git@github.com:Gogh-Co/Gogh.git ~/repos/gogh

    pushd ~/repos/gogh/themes \
        && TERMINAL=gnome-terminal ./gruvbox-dark.sh && popd

In `gnome-terminal`, Right click and open `Preferences`. Under `General`,
uncheck `Show menubar by default in new terminals` (note that this option is
not visible under Gnome). Select `Gruvbox Dark` profile. Under `Text` tab, set
`Custom font` to `Hack Regular 14` and uncheck `Terminal bell`. Under the
`Scrolling` tab, uncheck `Show scrollbar`. To make the theme the default, click
the little `v` on the theme name and select `Set as default`.

## Firefox

This installs a non-snap version of Firefox.

Add PPA:

    sudo add-apt-repository ppa:mozillateam/ppa

Lower priority of snap package:

    echo -e "Package: firefox*\nPin: release o=Ubuntu*\nPin-Priority: -1" \
        | sudo tee /etc/apt/preferences.d/mozilla-firefox

Install `firefox`:

    sudo apt install -y firefox

TODO unattended upgrades?

Optionally, remove snap:

    sudo snap remove firefox

## Ctags

Choose either `exuberant-ctags` or `universal-ctags`. I'm currently using
`universal-ctags`.

### exuberant-ctags

Install:

    sudo apt install -y exuberant-ctags

Link config:

    ln -sf ~/.dotfiles/ctags ~/.ctags

### universal-ctags

Install:

    sudo apt install -y universal-ctags && mkdir ~/.ctags.d/

Link config:

    ln -sf ~/.dotfiles/typescript.ctags ~/.ctags.d/typescript.ctags

## Python

Install `python3` and make `python` point to it:

    sudo apt install -y python3-pip python3-venv python-is-python3 python3-autopep8

Optionally, install `pgcli`

    sudo apt install -y libpq-dev && pip install --user pgcli

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

    ruby-install ruby 3.1 && echo 3.1 > ~/.ruby-version

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

## ssh-agent

Check status:

    ssh-add -L

Create systemd service:

    mkdir -p ~/.config/systemd/user \
        && ln -sf ~/.dotfiles/ssh-agent.service ~/.config/systemd/user/ssh-agent.service

Make it start automatically:

    systemctl --user enable ssh-agent \
        && systemctl --user start ssh-agent

Make the ssh client add keys to the running agent:

    echo "AddKeysToAgent yes" >>  ~/.ssh/config

## httpie

    curl -SsL https://packages.httpie.io/deb/KEY.gpg | sudo gpg --dearmour -o /etc/apt/trusted.gpg.d/httpie-key.gpg

    sudo curl -SsL -o /etc/apt/sources.list.d/httpie.list https://packages.httpie.io/deb/httpie.list

    sudo apt update \
        && sudo apt install httpie

## ack

Install:

    sudo apt install -y ack

Link config:

    ln -sf ~/.dotfiles/ackrc ~/.ackrc

## Extras

    sudo apt install -y jq

    sudo apt install -y fzf

    sudo apt install -y pcmanfm \
        && xdg-mime default pcmanfm.desktop inode/directory

    sudo apt install -y sxiv \
        && xdg-mime default sxiv.desktop image/gif \
        && xdg-mime default sxiv.desktop image/jpeg \
        && xdg-mime default sxiv.desktop image/jpg \
        && xdg-mime default sxiv.desktop image/png

    sudo apt install -y ksnip

    sudo apt install -y tree

    sudo apt install -y xclip

    sudo apt install -y highlight

    # SDKMAN (without touching my dotfiles)

    curl -s "https://get.sdkman.io?rcupdate=false" | bash \
        && source ~/.bash_local

    sdk install java

    sdk install maven

    sudo wget https://raw.github.com/juven/maven-bash-completion/master/bash_completion.bash \
        --output-document /etc/bash_completion.d/mvn

    # fnm (without touching my dotfiles)
    # NOT TESTED!
    curl -fsSL https://fnm.vercel.app/install | bash -s -- --skip-shell

    fnm completions --shell bash | sudo tee /etc/bash_completion.d/fnm > /dev/null

    fnm install 18

    ln -sf ~/.dotfiles/scripts/urlencode ~/bin/urlencode \
        && ln -sf ~/.dotfiles/scripts/google ~/bin/google \
        && ln -sf ~/.dotfiles/scripts/mute ~/bin/mute \
        && ln -sf ~/.dotfiles/scripts/unmute ~/bin/unmute

## Hacks

### Set key repeat rate etc

    sudo apt install inputplug

    cp ~/.dotfiles/skel/xsessionrc ~/.xsessionrc \
        && cat ~/.xsessionrc

Note that `.xsessionrc` is a dotfile specific to Debian (and its derivatives).

### Touchpad tapping

List all input devices:

    xinput

Likely, it is the input named something with *Synaptics*

    xinput list --name-only | grep -i synaptics

If this is correct, test it out by doing:

    xinput set-prop "$(xinput list --name-only | grep -i synaptics)" "libinput Tapping Enabled" 1

Make it persistent:

    echo xinput set-prop \""$(xinput list --name-only | grep -i synaptics)"\" \"libinput Tapping Enabled\" 1 \
        >> ~/.xsessionrc

