#!/usr/bin/env bash
# shellcheck disable=SC1090,SC1091

# TODO
# Show status in terminal header (eg vim is editing file, or which dir I'm currently in)
# Somewhere, maybe not in this file, add -d by default to `git difftool`
# Spelling
# What to do with git and aliases. Keep using gd, gc etc or use git aliases instead? Both?

case $- in
  *i*) ;;
  *) return;;
esac

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
  . /etc/bash_completion
fi

# By default, C-w behaves a bit strange in vi mode.
#
#   echo foo bar # C-w here, would not do anything (either in command or insert mode)
#       ^
#
# Rebinding C-w seem to do the trick. But before this can be done,
# we have to do either `stty werase undef`, or put `set bind-tty-special-chars off`
# in `.inputrc`.
bind -m vi-insert '"\C-w":unix-word-rubout'
# I don't have this mapped for regular vim, unbind it.
bind -m vi-command -r "\C-w"

HISTSIZE=5000
HISTFILESIZE=10000
# TODO Should I have ignoreboth:erasedups?
HISTCONTROL=ignoreboth

# Will for example suggest --allow-empty when doing completion for git commit
export GIT_COMPLETION_SHOW_ALL=1
export EDITOR="vim"

# Force loading of git completion function
if ! declare -f __git_complete > /dev/null; then
  bash_completion=$(pkg-config --variable=completionsdir bash-completion 2>/dev/null) ||
    bash_completion='/usr/share/bash-completion/completions'
  test -f "${bash_completion}/git" && . "${bash_completion}/git"
fi

alias o='xdg-open "$@" 2> /dev/null'
alias fmt="fmt --width=80"

alias grep="grep --color=auto"
alias ls="ls --color=auto"

# Set up git aliases and auto completion (if necessary)
if declare -f __git_complete > /dev/null; then
  alias g="git"
  __git_complete g __git_main

  alias ga="git add"
  __git_complete ga _git_add

  alias gd="git diff"
  __git_complete gd _git_diff

  alias gst="git status"
  alias gc="git commit -v"
  alias gc!="git commit -v --amend"
  alias gca="git commit -v -a"
  alias gca!="git commit -v -a --amend"
fi

PS1='\[\e[0;32m\][\[\e[0;32m\]\w\[\e[0;32m\]]\n\[\e[0;34m\]-\[\e[0;34m\]> \[\e[0m\]\$ \[\e[0m\]'

set -o vi

# Turns off terminal suspend feature (<C-s> which freezes everything)
stty -ixon

if [ -f ~/.dircolors ]; then
  eval "$(dircolors ~/.dircolors)"
fi

command -v direnv > /dev/null && eval "$(direnv hook bash)"

# The idea here is to have this file contain stuff that is very specific
# to a particular computer, or things that I simply don't want to put
# under version control. That also means that this file typically should
# not be symlinked. I have a template for it in `skel` dir that can be
# used as a starter.
if [ -f ~/.bash_local ]; then
  . ~/.bash_local
fi
