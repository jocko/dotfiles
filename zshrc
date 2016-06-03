HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

[ -d /usr/local/share/zsh-completions ] && fpath=(/usr/local/share/zsh-completions $fpath)

export EDITOR="vim"
export CLICOLOR=1
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

alias ll='ls -alh --color=auto'
alias ls='ls --color=auto'
alias curl='noglob curl'
alias b='bundle'
alias which="which -a"
alias g="git"
alias ga="git add"
alias gc="git commit -v"
alias gc!="git commit -v --amend"
alias gca="git commit -v -a"
alias gca!="git commit -v -a --amend"
alias gco="git checkout"
alias gd="git diff"
alias glog="git log --graph --decorate --oneline"
alias gloga="git log --graph --decorate --oneline --all"
alias gst="git status"
alias gup="git pull --rebase"
alias gp="git push"
alias fig="docker-compose"
alias tmux="tmux -2"
alias df="df -h"
alias du="du -h -d 0"
alias grep="grep --color=auto"
alias md="mkdir -p"
alias ip="curl icanhazip.com"
alias pyserv="python2 -m SimpleHTTPServer"
alias xx="atool -x"
alias t="tail -f"
alias urlencode='python2 -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'

function mkcd() {
  mkdir -p "$1" && cd "$1";
}

function psg() {
  ps x | grep $1 | grep -v grep
}

LS_COLORS='rs=0:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:';
export LS_COLORS

autoload -U compinit 
compinit

typeset -U path
path=(~/bin $path)

compdef sshrc=ssh

zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' \
  'm:{a-z\-}={A-Z\_}' \
  'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' \
  'r:[[:ascii:]]||[[:ascii:]]=** r:|=* m:{a-z\-}={A-Z\_}'

zstyle ':completion:*:*:*:*:processes' command 'ps -u $LOGNAME -o pid,user,command -w'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=32=01'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

autoload -U colors
colors

vim_ins_mode="%{$fg[yellow]%}%{$reset_color%}"
vim_cmd_mode="%{$fg[yellow]%}-- CMD --%{$reset_color%}"
vim_mode=$vim_ins_mode

function zle-keymap-select {
  vim_mode="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
  zle reset-prompt
}
zle -N zle-keymap-select

function zle-line-finish {
  vim_mode=$vim_ins_mode
}
zle -N zle-line-finish

# Fix a bug when you C-c in CMD mode and you'd be prompted with CMD mode indicator, while in fact you would be in INS mode
# Fixed by catching SIGINT (C-c), set vim_mode to INS and then repropagate the SIGINT, so if anything else depends on it, we will not break it
# Thanks Ron! (see comments)
function TRAPINT() {
  vim_mode=$vim_ins_mode
  return $(( 128 + $1 ))
} 

zmodload zsh/datetime

function precmd() {
  print_title "%~"

  ((cmd_duration = $EPOCHSECONDS - ${cmd_start:-$EPOCHSECONDS}))
  if (($cmd_duration > 15)); then
    print "$fg[yellow]${cmd_duration}s$reset_color"
  fi
  unset cmd_start
}

function print_title() {
  print -Pn '\e]0;$1\a'
}

function preexec() {
  print_title "$PWD:t: $2"

  cmd_start=$EPOCHSECONDS
}

# REPORTTIME=15
# TIMEFMT="$fg[magenta]'%J'$reset_color time: $fg[green]%*Es$reset_color, cpu: $fg[green]%P$reset_color"

function ding() {
  notify-send "$history[$HISTCMD]"
}

setopt prompt_subst
PROMPT='%{$fg[yellow]%}%{$fg[green]%}[%~]
%{$fg[blue]%}-> %{$fg[white]%}%# %{$reset_color%}'
RPROMPT='%{$(echotc UP 1)%}${vim_mode}%{$(echotc DO 1)%}'

setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_find_no_dups
setopt share_history
#setopt extendedglob
#unsetopt autocd beep
# bindkey -e
bindkey -v
KEYTIMEOUT=1
bindkey "^?" backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
#zstyle :compinstall filename '/home/jocko/.zshrc'
#autoload -Uz compinit
#compinit
# End of lines added by compinstall

FZF_DEFAULT_COMMAND='ag -g ""'
export FZF_DEFAULT_OPTS="--exact"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# . `brew --prefix`/etc/profile.d/z.sh
# unalias z 2> /dev/null
# z() {
#   if [[ -z "$*" ]]; then
#     cd "$(_z -l 2>&1 | fzf +s --tac | sed 's/^[0-9,.]* *//')"
#   else
#     _z "$@"
#   fi
# }

[ -f ~/.zsh_aliases ] && source ~/.zsh_aliases

