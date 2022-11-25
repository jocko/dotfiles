#!/usr/bin/env bash
# shellcheck disable=SC1090,SC1091

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
HISTCONTROL=ignoredups:erasedups
HISTIGNORE='ls:cd:cd *'
# shopt -s histverify
# PROMPT_COMMAND="history -n; history -w; history -c; history -r; $PROMPT_COMMAND"

# Allows to use ** when globbing. For example, ls **/*.rb (i.e. similar
# to find . -name '*.rb'). When globstar is off, ** can still be used,
# but behaves like * (i.e. only a single directory, not recursive). Not
# sure if particular useful, but will evaluate.
shopt -s globstar

# Will for example suggest --allow-empty when doing completion for git commit
export GIT_COMPLETION_SHOW_ALL=1
export EDITOR="vim"

# Force loading of git completion function
if ! declare -f __git_complete > /dev/null; then
  bash_completion=$(pkg-config --variable=completionsdir bash-completion 2>/dev/null) ||
    bash_completion='/usr/share/bash-completion/completions'
  test -f "${bash_completion}/git" && . "${bash_completion}/git"
fi

alias fmt="fmt --width=80"
alias grep="grep --color=auto"
alias ls="ls --color=auto"

# Set up git aliases and auto completion (if necessary)
# TODO Consider using git aliases instead. Together with `g` alias, will give `g d` instead of `gd` etc
if declare -f __git_complete > /dev/null; then
  alias g="git"
  __git_complete g __git_main

  alias ga="git add"
  __git_complete ga _git_add

  alias gd="git diff"
  __git_complete gd _git_diff

  # Currently, this does not work fully. For example: `gp origin <Tab>` gives no suggestions
  # alias gp="git push"
  # __git_complete gp _git_push

  alias gst="git status"
  alias gc="git commit -v"
  alias gc!="git commit -v --amend"
  alias gca="git commit -v -a"
  alias gca!="git commit -v -a --amend"
fi

# TODO
# Fix Ctrl-R (fzf probably)
# Show status in terminal header (eg vim is editing file)
# Somewhere, maybe not in this file, add -d by default to `git difftool`
# Spelling

PS1='\[\e[0;32m\][\[\e[0;32m\]\w\[\e[0;32m\]]\n\[\e[0;34m\]-\[\e[0;34m\]> \[\e[0m\]\$ \[\e[0m\]'
# PS1='\[\e[0;32m\][\[\e[0;32m\]\w\[\e[0;32m\]]\n\[\e[0;34m\]> \[\e[0m\]\$ \[\e[0m\]'

set -o vi

# TODO Better mnemonics, or probably abandon this. Try to extract the fzf part, maybe provide
# aliases, and/or functions (with mappings?).
# gsw() {
#   git rev-parse HEAD > /dev/null 2>&1 || return

#   git branch -v --sort=-committerdate |
#     grep -vF '*' |
#     fzf |
#     tr -s ' ' |
#     cut -d ' ' -f2 |
#     xargs -r git switch
# }

# gadd() {
#   git rev-parse HEAD > /dev/null 2>&1 || return

#   git ls-files -m -o --exclude-standard |
#     fzf -m |
#     xargs -r git add
# }

# gres() {
#   git rev-parse HEAD > /dev/null 2>&1 || return

#   git diff --name-only --cached |
#     fzf -m |
#     xargs -r git restore --staged
# }

# fzf-down() {
#   fzf --height 50% --min-height 20 --border --bind ctrl-/:toggle-preview "$@"
# }

# _gh() {
#   git rev-parse HEAD > /dev/null 2>&1 || return

#   git log --date=short --format="%C(green)%C(bold)%cd %C(auto)%h%d %s (%an)" --graph --color=always |
#   fzf-down --ansi --no-sort --reverse --multi --bind 'ctrl-s:toggle-sort' \
#     --header 'Press CTRL-S to toggle sort' \
#     --preview 'grep -o "[a-f0-9]\{7,\}" <<< {} | xargs git show --color=always' |
#   grep -o "[a-f0-9]\{7,\}"
# }

# Turns off terminal suspend feature (<C-s> which freezes everything)
stty -ixon

if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

if [ -f ~/.bash_local ]; then
  . ~/.bash_local
fi
