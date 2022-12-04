# My dotfiles

Tested on Ubuntu 22.04

## Bootstrap

Make sure `apt` is up to date and that `git` is installed.

If necessary, generate a new SSH key:

    ssh-keygen -t ed25519 -C "<email>"

Clone the dotfiles repo:

    git clone git@github.com:jocko/dotfiles.git ~/.dotfiles

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

    ln -sf ~/.dotfiles/vimrc ~/.vimrc

Install Vundle:

    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

Start `vim` and run `:PluginInstall`.

Now, rest of the commands can be executed via slime from inside
vim. Launch a `:terminal` and do `<C-c><C-c>` below:

    sudo pwd

Link custom vim `after`:

    ln -sf ~/.dotfiles/vim/after ~/.vim/after

Create dirs in home:

    mkdir ~/bin ~/repos

Install essential packages:

    sudo apt install -y build-essential curl direnv exuberant-ctags

Link ctags config:

    ln -sf ~/.dotfiles/ctags ~/.ctags

Optionally, copy skeleton file(s):

    cp ~/.dotfiles/skel/bash_local ~/.bash_local \
        && cat ~/.bash_local

Optionally, symlink dircolors:

    ln -sf ~/.dotfiles/dircolors ~/.dircolors

## Terminal Emulator

Install font of choice:

    sudo apt install -y fonts-hack-ttf

### xfce4-terminal

Install `xfce4-terminal` and make it the default:

    sudo apt install -y xfce4-terminal \
        && sudo update-alternatives --config x-terminal-emulator

Install color scheme:

    cp ~/.dotfiles/skel/xfce4-gruvbox-dark.theme \
        ~/.local/share/xfce4/terminal/colorschemes/gruvbox-dark.theme

In `xfce4-terminal`, right click and uncheck `Show Menubar`. Right click
again and open `Preferences...`. Under the `Appearance` tab, set font to
`Hack Regular 14`. Under the `Colors` tab, load preset `gruvbox dark`.

### GNOME Terminal

Install `gnome-terminal` and make it the default (by default, it is the default):

    sudo apt install -y gnome-terminal \
        && sudo update-alternatives --config x-terminal-emulator

Install color scheme:

    git clone git@github.com:Gogh-Co/Gogh.git ~/repos

    pushd ~/repos/Gogh/themes \
        && ./gruvbox-dark.sh && popd

In `gnome-terminal`, Right click and open `Preferences`. Under `General`,
uncheck `Show menubar by default in new terminals`. Select `Gruvbox
Dark` profile. Under `Text` tab, set `Custom font` to `Hack Regular 14`
and uncheck `Terminal bell`. Under the `Scrolling` tab, uncheck `Show
scrollbar`. To make the theme the default, click the little `v` on the
theme name and select `Set as default`.

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

    ln -sf ~/.dotfiles/i3.config ~/.config/i3/config 

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

TODO install dunst

    ...

Link config:

    mkdir -p ~/.config/dunst && ln -sf ~/.dotfiles/dunstrc ~/.config/dunst/dunstrc

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

    ln -sf ~/.dotfiles/scripts/urlencode ~/bin/urlencode \
        && ln -sf ~/.dotfiles/scripts/google ~/bin/google

## Hacks

### Map caps-lock to escape

Gnome has its own way of doing things. I'm documenting it here just in case.

    dconf write "/org/gnome/desktop/input-sources/xkb-options" "['caps:swapescape']"

For i3, we can add a line to `~/.xsessionrc`:

    echo "setxkbmap -option caps:escape" >> ~/.xsessionrc

Note that `.xsessionrc` is a dotifle specific to Debian (and its derivatives).

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

### User lingering

User lingering supposedly allows processes to run even after the user
has logged out. My problem with this was related to environment variables
and how they just kept on piling up. As an example, sourcing the default
`~/.profile` will add two new entries, `~/bin` and `~/.local/bin`, to the
`$PATH`. Doing this multiple times is probably quite harmless, but very
much not something I want. Fortunately, the feature can be disabled.

Check if user lingering is enabled:

    loginctl show-user 1000 --property=Linger

If so, disable it:

    loginctl disable-linger 1000

TODO Read up on user lingering. Also, how does it relate to `KillUserProcesses`?

    loginctl show-user --property=KillUserProcesses

## Notes

### Command-line

Use the `o` alias (aliased to `xdg-open`).

Use pgrep and pkill to look up and signal processes using pattern (in
package `procps`).

Use `git switch -` to switch to previous branch.

Use `apt-file search pattern` to search for package providing `pattern`
(e.g. `apt-file search [/usr/bin/]pgrep`).

Use `dpkg -L package` to list files installed from `package`.

Use `apt show package` to show info about `package`.

### Vim


Yank something with y. In insert mode, paste with <C-r>0

In insert mode, evaluate something and insert result with <C-r>= (e.g. <C-r>=60*5)

Make lower- & upper case with gu and gU, eg gUw

Execute command for visual selection, e.g. norm I// (i.e. comment out selected lines)

{ "foo": 42 }

:fin[d] to open files. Supports globbing, e.g. `**core<TAB>` or `*.clj<TAB>`

:b <something-unique-for-buffer> to go to open buffer (:ls lists open buffers)

Filter/pipe to external program, e.g. current line with :.!jq '.' (jq -c '.' makes json compact again)
{"answer":42}

To filter but keep original, copy to a new line before filter (i.e. yyp, then filter on new line)
{"answer":42}

Use / to search, C-g/C-t navigates between matches

Use gf to go to file under cursor. [<C-i> to goto definition. [i or :is[earch] /pattern to show first line that contains keyword. Study this! => https://vimways.org/2018/death-by-a-thousand-files/

Useful links
https://www.integralist.co.uk/posts/vim/
https://gist.github.com/romainl/4b9f139d2a8694612b924322de1025ce

gE  go backwards to end of prev word
gI  like "I", but always go to col 1
J   join line
gJ  join with next line, no space
K   look up keyword under cursor (might be useful if configured)
~   switches case of char under cursor
!!  in normal mode, same as :.!
gx  xdg-open thing under cursor

Learn :vimgrep & :arglist (see vimcasts)
