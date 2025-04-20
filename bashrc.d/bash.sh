HISTSIZE=5000
HISTFILESIZE=10000
HISTCONTROL=ignoreboth

# Shows pwd in terminal title
function my_prompt_command() {
  echo -en "\033]0;$(dirs)\a"
}
PROMPT_COMMAND=my_prompt_command

PS1='\e[33m[\w]\e[0m\n-> $ '

# Turns off terminal suspend feature (<C-s> which freezes everything)
stty -ixon

if [ -f ~/.dircolors ]; then
  eval "$(dircolors ~/.dircolors)"
fi

command -v direnv > /dev/null && eval "$(direnv hook bash)"

command -v highlight > /dev/null && export LESSOPEN="| highlight %s --out-format ansi --force --no-trailing-nl"
export LESS=" -R"
