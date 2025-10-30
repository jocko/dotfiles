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

Install chruby:

    ~/.dotfiles/install-chruby.sh

Installing older rubies require some tweaking.

Install gcc-14:

    sudo pacman -S --noconfirm gcc14

Ruby 3.1:

    ruby-install ruby 3.1 -- --with-gcc=gcc-14

Ruby 3.0:

    ruby-install -M https://ftp.ruby-lang.org/pub/ruby ruby 3.0.7 -- --with-gcc=gcc-14

Ruby 2.7:

    sudo pacman -S --noconfirm openssl-1.1

    CPPFLAGS="-I/usr/include/openssl-1.1" LDFLAGS="-L/usr/lib/openssl-1.1" \
        ruby-install -M https://ftp.ruby-lang.org/pub/ruby ruby 2.7.8 -- --with-gcc=gcc-14 

TODO Is --no-ri --no-rdoc needed for older rubies?

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

