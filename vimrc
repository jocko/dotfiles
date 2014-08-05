call pathogen#infect()
call pathogen#helptags()

set tabstop=2 shiftwidth=2
set expandtab

let mapleader=" "

set number

" Edit the alternate file
nnoremap <leader><leader> <c-^>

" Show invisibles
set list
set listchars=tab:▸\ ,eol:¬
" Show trailing ws
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red

set incsearch
set ignorecase
set smartcase

" Theme
if !has("gui_running")
  let g:gruvbox_italic=0
endif
colorscheme gruvbox
set background=dark

" Neocomplete
let g:neocomplete#enable_at_startup = 1
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
