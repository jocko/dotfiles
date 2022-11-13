#!/bin/bash
# shellcheck disable=SC1090,SC1091

case $- in
  *i*) ;;
  *) return;;
esac

_have() { type "$1" &>/dev/null; }

# TODO Are aliases kind of lame?
# alias g="git"
# alias ga="git add"
# alias gc="git commit -v"
# alias gc!="git commit -v --amend"
# alias gca="git commit -v -a"
# alias gca!="git commit -v -a --amend"
# alias gco="git checkout"
# alias gd="git diff"
# alias gst="git status"
# alias gp="git push"

PS1='\[\e[0;32m\][\[\e[0;32m\]\w\[\e[0;32m\]]\n\[\e[0;34m\]-\[\e[0;34m\]> \[\e[0m\]\$ \[\e[0m\]'

set -o vi

# TODO Experiment with this...
# https://polothy.github.io/post/2019-08-19-fzf-git-checkout/
# https://gist.github.com/junegunn/8b572b8d4b5eddd8b85e5f4d40f17236
# https://github.com/junegunn/fzf/wiki/Examples#git


if [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi

if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

if [ -f ~/.bash_local ]; then
  . ~/.bash_local
fi
