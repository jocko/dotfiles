HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

export EDITOR="vim"

alias ls="ls --color -F"
alias ll="ls --color -lh"

alias yain='yaourt -S'
alias pacupg="sudo pacman -Syu"
alias pacin="sudo pacman -S"

LS_COLORS='rs=0:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:';
export LS_COLORS

autoload -U compinit 
compinit

typeset -U path
path=(~/bin $path)

zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

autoload -U colors
colors

PROMPT="%{$fg[yellow]%}[%*] %{$fg[green]%}[%~]
%{$fg[blue]%}-> %{$fg[white]%}%# %{$reset_color%}"

setopt HIST_IGNORE_DUPS
#setopt extendedglob
#unsetopt autocd beep
bindkey -e
#zstyle :compinstall filename '/home/jocko/.zshrc'
#autoload -Uz compinit
#compinit
# End of lines added by compinstall
source /usr/share/chruby/chruby.sh
source /usr/share/chruby/auto.sh
chruby ruby-2.1.7

source /etc/profile.d/fzf.zsh
