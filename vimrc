filetype off

set rtp+=~/.vim/bundle/Vundle.vim
if filereadable(expand("~/.vim/bundle/Vundle.vim/.gitignore"))
  call vundle#begin()

  Plugin 'VundleVim/Vundle.vim'

  Plugin 'tpope/vim-fugitive'

  Plugin 'jpalardy/vim-slime'
  Plugin 'ludovicchabant/vim-gutentags'

  " Themes
  Plugin 'morhetz/gruvbox'
  Plugin 'liuchengxu/space-vim-dark'
  Plugin 'arcticicestudio/nord-vim'


  " Language specific stuff
  Plugin 'vim-ruby/vim-ruby'
  Plugin 'leafgarland/typescript-vim'
  Plugin 'maxmellon/vim-jsx-pretty'
  Plugin 'tpope/vim-fireplace'

  " Automatically add end statements. `end` after `def` in Ruby for example
  Plugin 'tpope/vim-endwise.git'
  " Comment stuff out
  Plugin 'tpope/vim-commentary'
  " Handy mappings (e.g ]q for navigating quickfix list)
  Plugin 'tpope/vim-unimpaired'
  " Turn off syntax for large files
  Plugin 'fracpete/vim-large-files'

  Plugin 'mileszs/ack.vim'
  Plugin 'unblevable/quick-scope'

  " TODO Evaluate
  " Plugin 'mhinz/vim-signify'
  " Plugin 'idanarye/vim-merginal'
  " Plugin 'sbdchd/neoformat'

  call vundle#end()
endif

filetype plugin indent on
syntax enable

if !executable('ctags')
  let g:gutentags_dont_load = 1
endif

" Colorscheme overrides, need to appear before we source any
" colorscheme. Since we do this with autocommands, it is possible to
" try out new themes (e.g. :color gruvbox) and not have these overrides
" carry over to the next theme . Also, the overrides are scoped to a
" specific theme.
autocmd ColorScheme nord highlight IncSearch cterm=NONE ctermfg=0 ctermbg=11
autocmd ColorScheme gruvbox highlight IncSearch cterm=NONE ctermfg=0 ctermbg=9

if filereadable(expand("~/.vim/bundle/gruvbox/README.md"))
  " Required for some themes (e.g. gruvbox)
  set background=dark
  color gruvbox
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
" Evaluate. Will make '-' part of keywords
" Result => Didn't work as well as I would have thought. For example, unable
" to auto complete JIRAs in branch names on the form SER-1234-make-teapots
" set iskeyword+=\-
augroup trailing
  au!
  au InsertEnter * :set listchars-=trail:·
  au InsertLeave * :set listchars+=trail:·
augroup END

" Plugin config
let g:http_client_focus_output_window = 0
let g:http_client_result_vsplit = 0
let g:http_client_preserve_responses = 1
let g:slime_default_config = {"sessionname": "vim", "windowname": "0"}
let g:slime_python_ipython = 1
let g:pandoc#modules#disabled = ["folding"]
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
let g:merginal_splitType = ''

set undodir=~/.vim/undo
if !isdirectory(expand(&undodir))
  call mkdir(expand(&undodir), "p")
endif

set directory=~/.vim/swap//
if !isdirectory(expand(&directory))
  call mkdir(expand(&directory), "p")
endif

runtime! macros/matchit.vim

" Mark occurrences (note: <raise>-f => *)
" nnoremap <Leader>f :let @/='\V\<'.escape(expand('<cword>'), '\').'\>'<cr>:set hls<cr>
nnoremap <Leader>f :let @/='\V\<'.escape(expand('<cword>'), '\').'\>'<cr>

" Clear hlsearch automatically
augroup vimrc-incsearch-highlight
  autocmd!
  autocmd CmdlineEnter [/\?] :set hlsearch
  autocmd CmdlineLeave [/\?] :set nohlsearch
augroup END

" vim-signify hunk text object
omap ic <plug>(signify-motion-inner-pending)
xmap ic <plug>(signify-motion-inner-visual)
omap ac <plug>(signify-motion-outer-pending)
xmap ac <plug>(signify-motion-outer-visual)
" vim-signify faster sign updates
set updatetime=100
" Don't shift entire editor when vim-signify kicks in
set signcolumn=yes

let maplocalleader=" "

autocmd FileType typescript,typescriptreact setlocal noexpandtab shiftwidth=4 tabstop=4
autocmd FileType json setlocal noexpandtab shiftwidth=2 tabstop=2
autocmd FileType markdown setlocal shiftwidth=4 tabstop=4

autocmd FileType typescript,typescriptreact setlocal path-=/usr/include path+=src/**
autocmd FileType typescript,typescriptreact setlocal complete-=i
autocmd FileType typescript,typescriptreact nnoremap <buffer> <LocalLeader>f :Neoformat prettierd<cr>

autocmd FileType python nnoremap <buffer> <LocalLeader>f :Neoformat autopep8<cr>

autocmd FileType gitcommit setlocal spell

autocmd FileType openscad setlocal autoindent smartindent

" TODO Figure this out
" autocmd FileType typescript,typescriptreact setlocal include=from
autocmd FileType typescript,typescriptreact setlocal define=\\(const\\)

" TODO Have this and let it contain the derpy fileplugin stuff (e.g. tabs in
" json)?
" if filereadable(expand('~/.vimrc.local'))
"   source ~/.vimrc.local
" endif

