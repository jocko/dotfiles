# shellcheck disable=SC1090,SC1091

case $- in
  *i*) ;;
  *) return;;
esac

_have() { type "$1" &>/dev/null; }


PS1='\[\e[0;32m\][\[\e[0;32m\]\w\[\e[0;32m\]]\n\[\e[0;34m\]-\[\e[0;34m\]> \[\e[0m\]\$ \[\e[0m\]'

set -o vi

if [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi

if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

if [ -f ~/.bash_local ]; then
  . ~/.bash_local
fi
