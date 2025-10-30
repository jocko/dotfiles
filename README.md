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

Install `sway`:

    ./install-sway.sh

Unless you are using a display manager, install `sway` auto start script:

    sudo cp -u ~/.dotfiles/profile.d/sway.sh /etc/profile.d/

Install Plasma:

    ./install-kde-plasma.sh

Log out.

Install AUR helper:

    ./install-aur-helper.sh

## Kagi

Login to `https://kagi.com/signin`. Right click address bar in Firefox and `Add
"Kagi Search"`. Open `about:preferences#search` and select `Kagi`.

## Git

TODO Import existing .ssh (create/extract from archive to preserve file permissions migh be best approach?)

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

    git config user.email "<email>"

    git remote set-url origin git@github.com:jocko/dotfiles.git

## Ruby

TODO Package ruby-erb needed for some rubies all of the sudden :/

Older rubies (pre 3.2) can be built with gcc14. First, install gcc14:

    sudo pacman -S --noconfirm gcc14

And then, with `ruby-build`:

    ruby-build --dir ruby-3.1.4 ~/.rubies -- CC=gcc-14

Or `mise`:

    mise settings --local set ruby.ruby_build_opts -- CC=gcc-14
    mise use ruby@3.1.4

## awscli

    sudo pacman -S --noconfirm aws-cli

    paru -S aws-session-manager-plugin

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

