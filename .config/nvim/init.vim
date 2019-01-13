if &shell =~# 'fish$'
	set shell=sh
endif

set termguicolors

set tabstop=4
set shiftwidth=4

set shortmess+=c
set noshowmode

set conceallevel=2
set nowrap

set undodir=~/.vimundo
set undofile

call plug#begin('~/.local/share/nvim/plugged')

" Vim enhancements
" - editing
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'

" :w !sudo tee % doesnt work in neovim
Plug 'lambdalisue/suda.vim'
command! W :w suda://%

" - navigation
Plug 'airblade/vim-rooter'
let g:rooter_change_directory_for_non_project_files = 'current'
let g:rooter_patterns = ['.git/', 'Cargo.tom', 'package.json']
let g:rooter_silent_chdir = 1

Plug 'junegunn/fzf.vim'
Plug 'chaoren/vim-wordmotion'
Plug 'Raimondi/delimitMate'
Plug 'w0rp/ale'
let g:ale_sign_column_always = 0
let g:ale_linters = {'rust': ['rls']}
let g:ale_rust_cargo_use_check = 1
let g:ale_rust_cargo_check_tests = 1
let g:ale_rust_cargo_use_clippy = 1
highligh ALEWarning ctermbg=none ctermfg=172 guifg=Orange cterm=underline gui=undercurl 
highligh ALEError ctermbg=none ctermfg=red guifg=Red cterm=underline gui=undercurl
" highligh ALEWarning cterm=undercurl
" highlight ALEError cterm=undercurl

" - autocomplete
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'

autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect

" - time tracking
Plug 'wakatime/vim-wakatime'

" Languages
" - interface
Plug 'autozimu/LanguageClient-neovim', {
	\ 'branch': 'next',
	\ 'do': 'bash install.sh',
	\ }
set hidden
let g:LanguageClient_serverCommands = {
	\ 'rust': ['env', 'CARGO_TARGET_DIR=~/.cache/cargo-target/rls', 'rls'],
	\ 'javascript': ['tsserver'],
	\ 'javascript.jsx': ['tcp://127.0.0.1:2089'],
	\ 'python': ['~/.local/bin/pyls'],
	\ 'cpp': ['ccls'],
	\ 'c': ['ccls']
	\ }
let g:LanguageClient_settingsPath = '~/.config/nvim/ls-settings.json'
let g:LanguageClient_autostart = 1
nnoremap <silent> gh :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> gr :call LanguageClient#textDocument_references()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
nnoremap <silent> <A-F> :call LanguageClient#textDocument_formatting()<CR>

Plug 'Shougo/echodoc.vim'
let g:echodoc_enable_at_startup = 1

Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'idanarye/vim-vebugger'

" - terminal
Plug 'dag/vim-fish'

Plug 'sheerun/vim-polyglot'

" - rust
let g:rustfmt_command = 'rustfmt +nightly'
let g:rustfmt_autosave = 1
let g:rustfmt_emit_files = 1
let g:rustfmt_fail_silently = 0
let g:rust_clip_command = 'xclip -select clipboard'

call plug#end()

let g:netrw_dirhistmax = 0
	
" Buffer navigation
nnoremap <silent> <C-l> :wincmd l<CR>
nnoremap <silent> <C-h> :wincmd h<CR>
nnoremap <silent> <C-k> :wincmd k<CR>
nnoremap <silent> <C-j> :wincmd j<CR>

" Use C-j/k to navigate nvm2 suggestions
inoremap <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"

" Config edit/reload
noremap <F12> :sp ~/.config/nvim/init.vim<CR>
noremap <F24> :so ~/.config/nvim/init.vim<CR>

noremap <F8> :ALENext<CR>
noremap <F20> :ALEPrevious<CR>

" H/L goto start/end of line
map H ^
map L $

noremap ; :

inoremap {<CR> {<CR>}<ESC>O


" Editor config
let mapleader = "\<Space>"
filetype plugin indent on
syntax on
set autoindent

set splitright
set splitbelow

set wildmenu
set wildmode=list:longest

set incsearch
set ignorecase
set smartcase
set gdefault
set shortmess+=s

set ruler
set number
set relativenumber

noremap <leader>p :r !xclip -select clipboard -o<CR>
noremap <leader>c :w !xclip -select clipboard -i<CR><CR>
nnoremap <leader><leader> <C-^>
nnoremap <leader>l :vsp #<CR>
nnoremap <leader>j :sp #<CR>
nmap <ESC> :noh<CR>

noremap <F1> <nop>

noremap <C-F> :Rg
noremap <C-P> :Files<CR>
noremap <leader>; :Buffers<CR>

" Use ripgrep instead of grep
if executable('rg')
	set grepprg=rg\ --no-heading\ --vimgrep
	set grepformat=%f:%l:%c:%m
endif


