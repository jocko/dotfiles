# My dotfiles

Tested on Fedora 42

## Bootstrap

Remove some unused packages:

    sudo dnf remove -y libreoffice-core

Make sure system is up to date:

    sudo dnf upgrade -y

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

    sudo dnf install -y vim-X11

Link the vim config:

    ln -sf ~/.dotfiles/vimrc ~/.vimrc

Note that `vim-X11` installs a `vimx` command, but no `vim`. Create `vim`
command:

    sudo ln -sf /usr/bin/gvim /usr/bin/vim

Install Vundle:

    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

Start `vim` and run `:PluginInstall`.

Now, rest of the commands can be executed via slime from inside
vim. Launch a `:term` and do `<C-c><C-c>` below:

    sudo pwd

Link bash and readline config:

    ln -sf ~/.dotfiles/bashrc.d ~/.bashrc.d \
        && ln -sf ~/.dotfiles/inputrc ~/.inputrc

Link git config:

    ln -sf ~/.dotfiles/gitconfig ~/.gitconfig \
        && ln -sf ~/.dotfiles/gitignore ~/.gitignore

Link custom vim `after`:

    ln -sf ~/.dotfiles/vim/after ~/.vim/after

Create dirs in home:

    mkdir ~/bin ~/repos ~/src ~/lab

TODO screenshot tool
Install "essential" packages:

    sudo dnf install --y direnv atool screen diceware htop \
        NetworkManager-tui pavucontrol make httpie ack

Symlink misc stuff:

    ln -sf ~/.dotfiles/dircolors ~/.dircolors

    ln -sf ~/.dotfiles/screenrc ~/.screenrc

    ln -sf ~/.dotfiles/ackrc ~/.ackrc

## Window Manager

Install `sway`:

    sudo dnf install -y sway

Link config:

    mkdir -p ~/.config/sway && ln -sf ~/.dotfiles/sway.config ~/.config/sway/config 

TODO Install `waybar`:

Install `dunst`:

    sudo dnf install -y dunst

Link config:

    mkdir -p ~/.config/dunst && ln -sf ~/.dotfiles/dunstrc ~/.config/dunst/dunstrc

Try it out:

    ln -sf ~/.dotfiles/scripts/ding ~/bin/ding && ~/bin/ding echo Ermahgerd!

## Terminal Emulator

Install font of choice:

    sudo dnf install -y source-foundry-hack-fonts

Install `xfce4-terminal`:

    sudo dnf install -y xfce4-terminal

Install color scheme:

    mkdir -p ~/.local/share/xfce4/terminal/colorschemes \
        && cp ~/.dotfiles/xfce4-gruvbox-dark.theme \
        ~/.local/share/xfce4/terminal/colorschemes/gruvbox-dark.theme

In `xfce4-terminal`, right click and open `Preferences...`. Under the `General`
tab. Set `Scrollbar is:` to `Disabled`. Uncheck `Show unsafe paste dialog`.
Under the `Appearance` tab, set font to `Hack Regular 14`. Uncheck `Display
menubar in new windows`. Under the `Colors` tab, load preset `gruvbox dark`.

### Kagi

Login to `https://kagi.com/signin`. Right click address bar in Firefox and `Add
"Kagi Search"`. Open `about:preferences#search` and select `Kagi`.

## Universal Ctags

Install:

    sudo apt install -y universal-ctags && mkdir ~/.ctags.d/

Link config:

    ln -sf ~/.dotfiles/typescript.ctags ~/.ctags.d/typescript.ctags

## Python

TODO python is already installed?
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

    ruby-install ruby 3.4 && echo 3.4 > ~/.ruby-version

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

## Fast Node Manager (fnm)

Install Fast Node Manager (`fnm`) without touching my dotfiles:

    curl -fsSL https://fnm.vercel.app/install | bash -s -- --skip-shell

    $HOME/.local/share/fnm/fnm completions --shell bash | sudo tee /etc/bash_completion.d/fnm > /dev/null

### Docker

Add repo:

     sudo dnf-3 config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo

Install `docker-ce`:

    sudo dnf install -y docker-ce

Start Docker Engine:

    sudo systemctl enable --now docker

Verify installation:

    sudo docker run hello-world

Add current user to `docker` group:

    sudo usermod -aG docker $USER

## Mullvad

Add Mullvad repo:

    sudo dnf config-manager addrepo \
        --from-repofile=https://repository.mullvad.net/rpm/stable/mullvad.repo

Install it:

    sudo dnf install -y mullvad-vpn

### TODO TMK

Install AVR toolchain:

    sudo dnf install -y avr-gcc avr-libc avrdude

Clone repo:

    git clone git@github.com:jocko/tmk_keyboard.git

    pushd ~/repos/tmk_keyboard/ \
        && git submodule update --init \
        && popd

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

### Printing

Configure printers using CUPS. Navigate to

    http://localhost:631

User should already belong to `lpadmin` group, so enter login credentials at
login prompt.

Under Administration, select `Add Printer`. TODO yada yada (EPSON Printer, driverless, 2.0.0)

TODO Evaluate drivers from Epson

