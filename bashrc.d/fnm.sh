if [ -f "$HOME/.local/share/fnm/fnm" ]; then
  export PATH="$HOME/.local/share/fnm:$PATH"
  eval "$(fnm env --use-on-cd)"
fi
