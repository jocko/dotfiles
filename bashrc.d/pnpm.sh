if [ -f "$HOME/.local/share/pnpm/pnpm" ]; then
  export PNPM_HOME="$HOME/.local/share/pnpm"
  export PATH="$PNPM_HOME:$PATH"

  # Output from 'pnpm completion bash' inlined
  ###-begin-pnpm-completion-###
  if type complete &>/dev/null; then
    _pnpm_completion () {
      local words cword
      if type _get_comp_words_by_ref &>/dev/null; then
        _get_comp_words_by_ref -n = -n @ -n : -w words -i cword
      else
        cword="$COMP_CWORD"
        words=("${COMP_WORDS[@]}")
      fi

      local si="$IFS"
      IFS=$'\n' COMPREPLY=($(COMP_CWORD="$cword" \
                             COMP_LINE="$COMP_LINE" \
                             COMP_POINT="$COMP_POINT" \
                             SHELL=bash \
                             pnpm completion-server -- "${words[@]}" \
                             2>/dev/null)) || return $?
      IFS="$si"

      if [ "$COMPREPLY" = "__tabtab_complete_files__" ]; then
        COMPREPLY=($(compgen -f -- "$cword"))
      fi

      if type __ltrim_colon_completions &>/dev/null; then
        __ltrim_colon_completions "${words[cword]}"
      fi
    }
    complete -o default -F _pnpm_completion pnpm
  fi
  ###-end-pnpm-completion-###
fi

