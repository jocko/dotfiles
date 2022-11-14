#!/usr/bin/env bash
# shellcheck disable=SC1090,SC1091

case $- in
  *i*) ;;
  *) return;;
esac

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
  . /etc/bash_completion
fi

HISTSIZE=5000
HISTFILESIZE=10000
HISTCONTROL=ignoredups:erasedups
shopt -s histappend
PROMPT_COMMAND="history -n; history -w; history -c; history -r; $PROMPT_COMMAND"

export GIT_COMPLETION_SHOW_ALL=1

function_exists() {
  declare -f -F $1 > /dev/null
  return $?
}

# for al in `git --list-cmds=alias`; do
#   alias g$al="git $al"

#   complete_func=_git_$(git --list-cmds=alias $al)
#   function_exists $complete_fnc && __git_complete g$al $complete_func
# done

# TODO
# Fix Ctrl-R (fzf probably)
# Show status in terminal header (eg vim is editing file)
# Somewhere, maybe not in this file, add -d by default to `git difftool`

# Definately need autocomplete for this
# alias g="git"
# alias ga="git add"
# alias gc="git commit -v"
# alias gc!="git commit -v --amend"
# alias gca="git commit -v -a"
# alias gca!="git commit -v -a --amend"
# # Use fzf? No, probably need another, sometimes want to use gco <file>. Consider "new" git commands, git switch & git restore
# alias gco="git checkout"
# alias gd="git diff"
# alias gst="git status"
# # This I need completion for
# alias gp="git push"

PS1='\[\e[0;32m\][\[\e[0;32m\]\w\[\e[0;32m\]]\n\[\e[0;34m\]-\[\e[0;34m\]> \[\e[0m\]\$ \[\e[0m\]'
# PS1='\[\e[0;32m\][\[\e[0;32m\]\w\[\e[0;32m\]]\n\[\e[0;34m\]> \[\e[0m\]\$ \[\e[0m\]'

set -o vi

# TODO Experiment with this...
# https://polothy.github.io/post/2019-08-19-fzf-git-checkout/
# https://gist.github.com/junegunn/8b572b8d4b5eddd8b85e5f4d40f17236
# https://github.com/junegunn/fzf/wiki/Examples#git


if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

if [ -f ~/.bash_local ]; then
  . ~/.bash_local
fi
