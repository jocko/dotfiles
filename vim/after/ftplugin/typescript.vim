setlocal noexpandtab shiftwidth=4 tabstop=4
setlocal include=from
setlocal path-=/usr/include
" setlocal path+=src/**

" https://gist.github.com/bfrg/a7343cb63b4a7170a140e9b5b9326b23
" https://github.com/vim/vim/blob/master/runtime/ftplugin/javascript.vim
" setlocal define=^\\s*var
      " \ '\(^\s*(*async\s\+function\|(*function\)'
      " \ .. '\|^\s*\(\*\|static\|async\|get\|set\|\i\+\.\)'
      " \ .. '\|^\s*\(\ze\i\+\)\(([^)]*).*{$\|\s*[:=,]\)'
      " \ .. '\|^\s*\(export\s\+\|export\s\+default\s\+\)*\(var\|let\|const\|function\|class\)'
      " \ .. '\|\<as\>'
