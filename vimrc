call pathogen#infect()
call pathogen#helptags()

set tabstop=2 shiftwidth=2
set expandtab 
 
set number

set hlsearch 
set incsearch
set ignorecase
set smartcase 

" Theme
if !has("gui_running")
  let g:gruvbox_italic=0
endif
colorscheme gruvbox
set background=dark

" Split navigation, ctrl-j, ctrl-k, etc
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

imap <Tab> <C-P>

