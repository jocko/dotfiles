# My dotfiles

Tested on Fedora 41

## Bootstrap

TODO Remove some unused packages:

    sudo dnf remove libreoffice-core # TODO --assumeyes ?

Make sure system is up to date:

    sudo dnf upgrade # TODO --assumeyes ?

Make sure that you are satisfied with the hostname (it will be used for
the SSH key):

    hostname

Or, set a new one:

    sudo hostname piglet

If necessary, generate a new SSH key (and add it to github):

    ssh-keygen -t ed25519

Clone the dotfiles repo:

    git clone git@github.com:jocko/dotfiles.git ~/.dotfiles

Since I might be using different emails across my computers, I don't want this
config in my dotfiles. Instead, I set this system wide:

    sudo git config --system user.email "<email>"

When it comes to vim, there are a couple of options. Package `vim-enhanced`
works okay, but doesn't have `xterm_clipboard` (can be checked in vim
by doing `:echo has('clipboard')`). Better option then is to install
`vim-X11`.

    sudo dnf install --assumeyes vim-X11

Link the vim config:

    ln -sf ~/.dotfiles/vimrc ~/.vimrc

Install Vundle:

    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

Start `gvim -v` and run `:PluginInstall`.

Now, rest of the commands can be executed via slime from inside
vim. Launch a `:terminal` and do `<C-c><C-c>` below:

    sudo pwd

Link bash and readline config:

    ln -sf ~/.dotfiles/bashrc ~/.bashrc && ln -sf ~/.dotfiles/inputrc ~/.inputrc

Link the git config:

    ln -sf ~/.dotfiles/gitconfig ~/.gitconfig \
        && ln -sf ~/.dotfiles/gitignore ~/.gitignore

Link custom vim `after`:

    ln -sf ~/.dotfiles/vim/after ~/.vim/after

Create dirs in home:

    mkdir ~/bin ~/repos ~/src ~/lab

Install essential packages:

    sudo dnf install --assumeyes direnv # TODO Maybe remove completely?

Optionally, copy skeleton file(s):

    cat ~/.dotfiles/skel/bash_local

    cp ~/.dotfiles/skel/bash_local ~/.bash_local \
        && cat ~/.bash_local

Optionally, symlink dircolors:

    ln -sf ~/.dotfiles/dircolors ~/.dircolors

## Window Manager

Install `i3`:

    sudo dnf install --assumeyes i3

Link config:

    mkdir -p ~/.config/i3 && ln -sf ~/.dotfiles/i3.config ~/.config/i3/config 

Install `rofi` (dmenu replacement)

    sudo apt install -y rofi

Install `xkblayout-state`

    sudo apt install -y libx11-dev

    git clone git@github.com:nonpop/xkblayout-state.git ~/src/xkblayout-state

    pushd ~/src/xkblayout-state && make && cp xkblayout-state ~/bin && popd

Install `i3blocks`:

    sudo apt install -y i3blocks fonts-font-awesome

Link config:

    ln -sf ~/.dotfiles/i3blocks.conf ~/.i3blocks.conf

Link `dunst` config (`dunst` is recommended by `i3`, so it should already be
installed):

    mkdir -p ~/.config/dunst && ln -sf ~/.dotfiles/dunstrc ~/.config/dunst/dunstrc

Try it out:

    ln -sf ~/.dotfiles/scripts/ding ~/bin/ding && ~/bin/ding echo Ermahgerd!

## Terminal Emulator

Install font of choice:

    sudo apt install -y fonts-hack-ttf

Choose either `xfce4-terminal` or `gnome-terminal`. They should work equally
well AFAIK.

### xfce4-terminal

Install `xfce4-terminal` and make it the default:

    sudo dnf install --assumeyes xfce4-terminal

Install color scheme:

    mkdir -p ~/.local/share/xfce4/terminal/colorschemes \
        && cp ~/.dotfiles/skel/xfce4-gruvbox-dark.theme \
        ~/.local/share/xfce4/terminal/colorschemes/gruvbox-dark.theme

In `xfce4-terminal`, right click and open `Preferences...`. Under the `General`
tab. Set `Scrollbar is:` to `Disabled`. Uncheck `Show unsafe paste dialog`.
Under the `Appearance` tab, set font to `Hack Regular 14`. Uncheck `Display
menubar in new windows`. Under the `Colors` tab, load preset `gruvbox dark`.

### GNOME Terminal

TODO Verify this section

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

Install keyring:

    wget -q https://packages.mozilla.org/apt/repo-signing-key.gpg -O- \
        | sudo tee /etc/apt/keyrings/packages.mozilla.org.asc > /dev/null

Configure repo:

    echo "deb [signed-by=/etc/apt/keyrings/packages.mozilla.org.asc] https://packages.mozilla.org/apt mozilla main" \
        | sudo tee -a /etc/apt/sources.list.d/mozilla.list > /dev/null

Lower priority of snap package:

    echo -e "Package: firefox*\nPin: release o=Ubuntu*\nPin-Priority: -1" \
        | sudo tee /etc/apt/preferences.d/mozilla-firefox

Uninstall snap package:

    sudo apt update && sudo apt remove -y firefox

Install `firefox`:

    sudo apt install -y firefox

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

Link `gem` config:

    ln -sf ~/.dotfiles/gemrc ~/.gemrc

Download `ruby-install` (note: version is static):

    wget -O ~/src/ruby-install.tar.gz https://github.com/postmodern/ruby-install/archive/v0.9.3.tar.gz \
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

TODO Probably install a later version
Install ruby 3.3 (note: might take a while):

    ruby-install ruby 3.3 && echo 3.3 > ~/.ruby-version

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
        && sudo apt install -y httpie

## ack

Install:

    sudo apt install -y ack

Link config:

    ln -sf ~/.dotfiles/ackrc ~/.ackrc

## Git PPA

Install latest stable Git version from PPA:

    sudo add-apt-repository ppa:git-core/ppa && sudo apt install -y git

## SDKMAN

Install SDKMAN (`sdk`) without touching my dotfiles:

    curl -s "https://get.sdkman.io?rcupdate=false" | bash \
        && source ~/.bash_local

    sdk install java

    sdk install maven

    sudo wget https://raw.github.com/juven/maven-bash-completion/master/bash_completion.bash \
        --output-document /etc/bash_completion.d/mvn

## Fast Node Manager (fnm)

Install Fast Node Manager (`fnm`) without touching my dotfiles:

    curl -fsSL https://fnm.vercel.app/install | bash -s -- --skip-shell

    fnm completions --shell bash | sudo tee /etc/bash_completion.d/fnm > /dev/null

    fnm install 22

## Jetbrains Toolbox App

Toolbox App is packaged as an AppImage, which requires FUSE:

    sudo apt install -y libfuse2

    curl -s  'https://data.services.jetbrains.com/products/releases?code=TBA&latest=true&type=release' \
        | jq -r '.TBA[0].downloads.linux.link' \
        | xargs wget -O /tmp/jetbrains-toolbox.tar.gz \
        && tar -xzvf /tmp/jetbrains-toolbox.tar.gz --strip-components=1 -C /tmp \
        && /tmp/jetbrains-toolbox \
        && ln -sf ~/.local/share/JetBrains/Toolbox/bin/jetbrains-toolbox ~/bin/jetbrains-toolbox 

## Desktop Utils

Setup file manager of choice:

    sudo apt install -y pcmanfm \
        && xdg-mime default pcmanfm.desktop inode/directory

Setup Simple X Image Viewer:

    sudo apt install -y sxiv \
        && xdg-mime default sxiv.desktop image/gif \
        && xdg-mime default sxiv.desktop image/jpeg \
        && xdg-mime default sxiv.desktop image/jpg \
        && xdg-mime default sxiv.desktop image/png

Setup (PDF) document viewer:

    sudo apt install zathura \
        && xdg-mime default org.pwmt.zathura.desktop application/pdf

Install screenshot tool:

    sudo apt install -y ksnip

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

## TODO


* kicad
* locate (sudo apt install plocate)
