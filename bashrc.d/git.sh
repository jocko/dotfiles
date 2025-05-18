# Will for example suggest --allow-empty when doing completion for git commit
export GIT_COMPLETION_SHOW_ALL=1
export GIT_PAGER="less -FX"

# Force loading of git completion function
if ! declare -f __git_complete > /dev/null; then
  bash_completion=$(pkg-config --variable=completionsdir bash-completion 2>/dev/null) ||
    bash_completion='/usr/share/bash-completion/completions'
  test -f "${bash_completion}/git" && . "${bash_completion}/git"
fi

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
