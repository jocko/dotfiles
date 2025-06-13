# My dotfiles

## Installing

After booting into live environment, connect to Internet by launching `iwctl`, then do:

    station wlan0 connect <SSID>

Start installer:

    archinstall

TODO

## Bootstrap

After install has finished and we have booted into the fresh system. Connect to
Internet:

    nmcli d wifi connect <SSID> --ask

Clone the dotfiles repo:

    git clone https://github.com/jocko/dotfiles.git ~/.dotfiles

Change directory to `~/.dotfiles` and do:

    ./bootstrap.sh

Install AUR helper:

    ./install-aur-helper.sh

## Kagi

Login to `https://kagi.com/signin`. Right click address bar in Firefox and `Add
"Kagi Search"`. Open `about:preferences#search` and select `Kagi`.

## Git

Make sure that you are satisfied with the hostname (it will be used for
the SSH key):

    hostnamectl hostname

Or, set a new one:

    sudo hostnamectl hostname piglet

If necessary, generate a new SSH key (and add it to github):

    ssh-keygen -t ed25519

Since I might be using different emails across my computers, I don't want this
config in my dotfiles. Instead, I set this system wide:

    sudo git config --system user.email "<email>"

TODO email for dotfiles should always be my personal one

Change origin of dotfiles repo:

    git remote set-url origin git@github.com:jocko/dotfiles.git

## Python

Install `python3` and make `python` point to it:

    sudo apt install -y python3-pip python3-venv python-is-python3 python3-autopep8

Optionally, install `pgcli`

    sudo apt install -y libpq-dev && pip install --user pgcli

## Ruby

Link `gem` config:

    ln -sf ~/.dotfiles/gemrc ~/.gemrc

Download `ruby-install` (note: version is static):

    wget -O ~/src/ruby-install.tar.gz https://github.com/postmodern/ruby-install/archive/v0.10.1.tar.gz \
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

    sudo apt install -y thunar \
        && xdg-mime default thunar.desktop inode/directory

In `Thunar`, open `Edit/Preferences`. Under `Display`, select `Remember view
settings for each folder`.

Hack for getting Firefox to respect my mime mapping above when opening downloads folder.

    mkdir -p ~/.local/share/dbus-1/services \
        && cp ~/.dotfiles/skel/org.freedesktop.FileManager1.service ~/.local/share/dbus-1/services/

Setup Simple X Image Viewer:

    sudo apt install -y sxiv \
        && xdg-mime default sxiv.desktop image/gif \
        && xdg-mime default sxiv.desktop image/jpeg \
        && xdg-mime default sxiv.desktop image/jpg \
        && xdg-mime default sxiv.desktop image/png

Setup (PDF) document viewer:

    sudo apt install -y zathura \
        && xdg-mime default org.pwmt.zathura.desktop application/pdf

Install screenshot tool:

    sudo apt install -y ksnip

Install graphical text editor:

    sudo apt install -y mousepad \
        && xdg-mime default org.xfce.mousepad.desktop text/plain

## Miscellaneous

### Set key repeat rate etc

    sudo apt install inputplug

    cp ~/.dotfiles/skel/xsessionrc ~/.xsessionrc \
        && cat ~/.xsessionrc

Note that `.xsessionrc` is a dotfile specific to Debian (and its derivatives).

### Touchpad tapping

TODO: find a better way to identify the touchpad

List all input devices:

    xinput

Likely, it is the input named something with *Synaptics*

    xinput list --name-only | grep -i touchpad

If this is correct, test it out by doing:

    xinput set-prop "$(xinput list --name-only | grep -i touchpad)" "libinput Tapping Enabled" 1

Make it persistent:

    echo xinput set-prop \""$(xinput list --name-only | grep -i synaptics)"\" \"libinput Tapping Enabled\" 1 \
        >> ~/.xsessionrc

### Kagi

Login to `https://kagi.com/signin`. Right click address bar in Firefox and `Add
"Kagi Search"`. Open `about:preferences#search` and select `Kagi`.

### Printing

Configure printers using CUPS. Navigate to

    http://localhost:631

User should already belong to `lpadmin` group, so enter login credentials at
login prompt.

Under Administration, select `Add Printer`. TODO yada yada (EPSON Printer, driverless, 2.0.0)

TODO Evaluate drivers from Epson

### TMK

    git clone ...

    git submodule update --init

Install AVR toolchain:

    sudo apt install -y gcc-avr avr-libc avrdude

TODO Linux udev rules

Teensy:

    wget -O /tmp/teensy.tar.gz https://www.pjrc.com/teensy/teensy_linux64.tar.gz \
        && mkdir /tmp/teensy \
        && tar -xzvf /tmp/teensy.tar.gz -C /tmp/teensy/

UDEV rules:

    wget -q https://www.pjrc.com/teensy/00-teensy.rules -O- \
        | sudo tee /etc/udev/rules.d/00-teensy.rules > /dev/null

TODO `hid_listen`

    wget -O ~/bin/hid_listen https://github.com/tmk/hid_listen/raw/master/binaries/hid_listen.linux \
        && chmod +x ~/bin/hid_listen

### KiCad

    sudo add-apt-repository ppa:kicad/kicad-8.0-releases \
        && sudo apt install -y kicad

### Docker

TODO Untested

Install keyring:

    sudo curl https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc

Configure repo:

    echo "deb [signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu $(. /etc/os-release && echo "$VERSION_CODENAME") stable" \
        | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

Install `docker-ce`:

    sudo apt update && sudo apt install -y docker-ce

Add user to `docker` group:

    sudo usermod -aG docker $USER

### Everything else

    sudo apt install -y apt-file diceware plocate htop meld highlight arandr \
        net-tools

    sudo apt install -y openscad

### TODO ssh config

Create versioned config file?

Make the ssh client add keys to the running agent:

    echo "AddKeysToAgent yes" >>  ~/.ssh/config
