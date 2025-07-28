filetype off

set rtp+=~/.vim/bundle/Vundle.vim
if filereadable(expand("~/.vim/bundle/Vundle.vim/.gitignore"))
  call vundle#begin()

  Plugin 'VundleVim/Vundle.vim'

  Plugin 'tpope/vim-fugitive'

  Plugin 'jpalardy/vim-slime'
  Plugin 'ludovicchabant/vim-gutentags'

  Plugin 'morhetz/gruvbox'

  " Language specific stuff
  Plugin 'vim-ruby/vim-ruby'
  Plugin 'leafgarland/typescript-vim'
  Plugin 'maxmellon/vim-jsx-pretty'
  Plugin 'tpope/vim-fireplace'

  " Automatically add end statements. `end` after `def` in Ruby for example
  Plugin 'tpope/vim-endwise.git'
  " Comment stuff out
  Plugin 'tpope/vim-commentary'
  " Turn off syntax for large files
  Plugin 'fracpete/vim-large-files'

  Plugin 'mileszs/ack.vim'
  Plugin 'unblevable/quick-scope'

  " TODO Evaluate
  Plugin 'mhinz/vim-signify' " Added back at 2025-04-09 - I was missing some kind of line wise indication that a line had been changed (or not)
  Plugin 'sbdchd/neoformat'
  Plugin 'tpope/vim-projectionist'
  Plugin 'yegappan/lsp'
  Plugin 'tpope/vim-dispatch'

  call vundle#end()
endif

filetype plugin indent on
syntax enable

if !executable('ctags')
  let g:gutentags_dont_load = 1
endif

" Automatically reread files that have changed outside vim
set autoread
augroup auto_checktime
  autocmd!
  " https://unix.stackexchange.com/questions/149209/refresh-changed-content-of-file-opened-in-vim/383044#383044
  " https://vi.stackexchange.com/questions/13692/prevent-focusgained-autocmd-running-in-command-line-editing-mode
  autocmd FocusGained,BufEnter,CursorHold,CursorHoldI *
          \ if mode() !~ '\v(c|r.?|!|t)' && getcmdwintype() == '' | checktime | endif
augroup END
" Fixes autoread for kitty, alacritty etc (taken from https://sw.kovidgoyal.net/kitty/faq)
let &t_fe = "\e[?1004h"
let &t_fd = "\e[?1004l"
execute "set <FocusGained>=\<Esc>[I"
execute "set <FocusLost>=\<Esc>[O"

let g:terminal_ansi_colors = [
  \'#282828', '#CC241D', '#98971A', '#D79921',
  \'#458588', '#B16286', '#689D6A', '#a89984',
  \'#fb4934', '#b8bb26', '#fabd2f', '#83a598',
  \'#d3869b', '#8ec07c', '#fe8019', '#FBF1C7' ]

highlight Terminal guibg='#282828'
highlight Terminal guifg='#ebdbb2'

" Colorscheme overrides, need to appear before we source any
" colorscheme. Since we do this with autocommands, it is possible to
" try out new themes (e.g. :color gruvbox) and not have these overrides
" carry over to the next theme . Also, the overrides are scoped to a
" specific theme.
autocmd ColorScheme gruvbox highlight IncSearch cterm=NONE ctermfg=0 ctermbg=9

if filereadable(expand("~/.vim/bundle/gruvbox/README.md"))
  " termguicolors is set according to instructions on
  " https://github.com/morhetz/gruvbox/wiki/Terminal-specific
  "
  " Not having this setting can cause a border around vim, in a slightly
  " different shade (how big this border is depends on how well the font size
  " aligns with the screen resolution)
  if (has("termguicolors"))
    set termguicolors
  endif

  " Required for some themes (e.g. gruvbox)
  set background=dark
  color gruvbox
endif

" Jump to the last known cursor position
augroup lastplace
  au!
  autocmd BufReadPost *
        \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit\|rebase'
        \ |   exe "normal! g`\""
        \ | endif
augroup END

set tabstop=2 shiftwidth=2
set expandtab
set number
set ttimeout
set ttimeoutlen=100
set hidden
set laststatus=2
set wildmenu
set wildmode=longest:lastused:full,full
set wildignorecase
set wildignore+=tags
if v:version >= 900
  set wildoptions=pum
endif
" Why do I keep changing this?
" set ignorecase
" set smartcase
set infercase
set tagcase=match
set scrolloff=5
set undofile
set backspace=indent,eol,nostop
set incsearch
" set hlsearch
set completeopt=longest,menu,preview
" This is useful, but throws off j/k movement with count (if there are any
" wrapped lines between). See gj/gk which operates on display lines instead.
" 2023-02-21 Evaluating using nowrap. Try to document why wrap is a good thing
" if I end up going back. Also, maybe have it set on a per filetype basis.
set nowrap
set nrformats=bin,hex,unsigned
set cursorline
set list
set listchars=tab:▸\ ,trail:·
set path-=/usr/include
set noshowcmd

augroup trailing
  au!
  au InsertEnter * :set listchars-=trail:·
  au InsertLeave * :set listchars+=trail:·
augroup END

" Plugin config
let g:slime_target = "vimterminal"
let g:slime_python_ipython = 1
let g:gutentags_define_advanced_commands = 1
let g:gutentags_add_default_project_roots = 0
let g:gutentags_project_root = ['package.json', 'Gemfile']
let g:gutentags_file_list_command = {
            \ 'markers': {
            \ 'package.json': 'ack -f --no-filter -t ts -t js',
            \ 'Gemfile': 'ack -f --no-filter -t ruby',
            \ },
            \ }
let g:gutentags_ctags_exclude_wildignore = 0
let g:gutentags_ctags_exclude = []
let g:gutentags_ctags_exclude += ['*.test.ts']
let g:gutentags_ctags_extra_args = []
let g:gutentags_ctags_extra_args += ['--TypeScript-kinds=-p']
let LargeFile = 1024 * 1024 * 1
let g:dispatch_no_maps = 1

set undodir=~/.vim/undo
if !isdirectory(expand(&undodir))
  call mkdir(expand(&undodir), "p")
endif

set directory=~/.vim/swap//
if !isdirectory(expand(&directory))
  call mkdir(expand(&directory), "p")
endif

runtime! macros/matchit.vim

" TODO Remove?
" Mark occurrences (note: <raise>-f => *)
" nnoremap <Leader>f :let @/='\V\<'.escape(expand('<cword>'), '\').'\>'<cr>:set hls<cr>
nnoremap <Leader>f :let @/='\V\<'.escape(expand('<cword>'), '\').'\>'<cr>

" TODO And this?
" Clear hlsearch automatically
augroup vimrc-incsearch-highlight
  autocmd!
  autocmd CmdlineEnter [/\?] :set hlsearch
  autocmd CmdlineLeave [/\?] :set nohlsearch
augroup END

" vim-signify faster sign updates
set updatetime=100
" Don't shift entire editor when vim-signify kicks in
set signcolumn=yes

let maplocalleader=" "

" autocmd FileType typescript,typescriptreact setlocal noexpandtab shiftwidth=4 tabstop=4
" autocmd FileType json setlocal noexpandtab shiftwidth=2 tabstop=2
autocmd FileType markdown setlocal shiftwidth=4 tabstop=4

autocmd FileType typescript,typescriptreact setlocal complete-=i
autocmd FileType typescript,typescriptreact nnoremap <buffer> <LocalLeader>f :Neoformat prettierd<cr>
autocmd FileType typescript,typescriptreact compiler eslint

autocmd FileType python nnoremap <buffer> <LocalLeader>f :Neoformat autopep8<cr>

autocmd FileType gitcommit setlocal spell

autocmd FileType openscad setlocal autoindent smartindent

" TODO Evaluate
nnoremap gs :Git<cr>
nnoremap gb :Git blame<cr>
nnoremap g<Space> :Ack! 
nnoremap mm :Make<cr>
" nnoremap <C-N> :cnext<cr>
" nnoremap <C-P> :cprev<cr>
nnoremap ]q :cnext<cr>
nnoremap ]Q :clast<cr>
nnoremap [q :cprev<cr>
nnoremap [Q :cfirst<cr>
iabbrev #b #!/usr/bin/env

" TODO Clean up
" let tsLspCmd = expand('$FNM_MULTISHELL_PATH') . '/bin/typescript-language-server'
let tsLspCmd = expand('$FNM_MULTISHELL_PATH') . '/bin/typescript-language-server'
if executable(tsLspCmd)
  let lspOpts = #{autoComplete: v:false}
  autocmd User LspSetup call LspOptionsSet(lspOpts)

  let lspServers = [#{
    \	  name: 'tsserver',
    \	  filetype: ['typescript', 'typescriptreact',],
    \	  path: tsLspCmd,
    \	  args: ['--stdio']
    \ }]
  autocmd User LspSetup call LspAddServer(lspServers)
endif

function! s:foo() 
  " TODO Does this work the same as regular version? For example, if I have a
  " split open and the target of the goto is open in another buffer, i get
  " switched to that buffer instead of opening in current
  nmap <buffer> <C-]> :LspGotoDefinition<CR>
  nnoremap <buffer> ]d :LspDiagNext<cr>
  nnoremap <buffer> [d :LspDiagPrev<cr>
  setlocal keywordprg=:LspHover
endfunction

augroup foo
  autocmd!
  autocmd User LspAttached call s:foo()
augroup END
" if executable('typescript-language-server')
"   call LspAddServer([#{name: 'tsserver'
"                  \   filetype: ['javascript', 'typescript'],
"                  \   path: 'typescript-language-server',
"                  \   args: ['--stdio']
"                  \ }])
" endif

" TODO Misc grep experimenting
let g:ackprg = "rg --vimgrep"
let g:ack_apply_qmappings = 0
let g:ack_apply_lmappings = 0
" let g:ack_qhandler = "botright copen | wincmd p"
