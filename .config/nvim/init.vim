if &shell =~# 'fish$'
	set shell=sh
endif

set termguicolors

set tabstop=2
set shiftwidth=2

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
Plug 'airblade/vim-gitgutter'

" :w !sudo tee % doesnt work in neovim
Plug 'lambdalisue/suda.vim'
command! W :w suda://%

" - navigation
Plug 'airblade/vim-rooter'
let g:rooter_change_directory_for_non_project_files = 'current'
let g:rooter_patterns = ['.git/', 'Cargo.toml', 'package.json', 'pom.xml']
let g:rooter_silent_chdir = 1

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'chaoren/vim-wordmotion'

Plug 'Raimondi/delimitMate'
let delimitMate_expand_cr = 1

" - autocomplete
set completeopt=noinsert,menuone,noselect

" - theme
Plug 'chriskempson/base16-vim'

" - time tracking
Plug 'wakatime/vim-wakatime'

" Languages
" - interface
Plug 'neoclide/coc.nvim', { 'do': { -> coc#util#build() } }

set hidden

" - code keybindings
inoremap <silent> <expr> <C-Space> coc#refresh()
inoremap <silent> <C-d> <C-O>:call CocActionAsync('showSignatureHelp')<CR>

nmap <silent> gh :call CocAction('doHover')<CR>
nmap <silent> ge <Plug>(coc-diagnostic-info)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> <F2> <Plug>(coc-rename)
nmap <silent> <A-F> <Plug>(coc-format)
nmap <silent> <C-I> <Plug>(coc-codeaction)
nmap <silent> <C-,> <Plug>(coc-fix-current)

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
nnoremap <silent> K :call <SID>show_documentation()<CR>


Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'idanarye/vim-vebugger'

Plug 'jparise/vim-graphql'
Plug 'thosakwe/vim-flutter'

Plug 'sheerun/vim-polyglot'

" - rust
let g:rustfmt_command = 'rustfmt +nightly'
let g:rustfmt_autosave = 1
let g:rustfmt_emit_files = 1
let g:rustfmt_fail_silently = 0
let g:rust_clip_command = 'xclip -select clipboard'

call plug#end()

function! WriteCreatingDirs()
	execute ':silent !mkdir -p %:h'
	write
endfunction
command! WW call WriteCreatingDirs()

command! -nargs=? E :e %:h/<args>

let g:netrw_dirhistmax = 0
	
" Buffer navigation
nmap <silent> <C-l> :wincmd l<CR>
nmap <silent> <C-h> :wincmd h<CR>
nmap <silent> <expr> <C-k> coc#util#has_float() && coc#util#float_scrollable() ? ":call coc#util#float_scroll(0)<CR>" : ":wincmd k<CR>"
imap <silent> <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"
nmap <silent> <expr> <C-j> coc#util#has_float() && coc#util#float_scrollable() ? ":call coc#util#float_scroll(1)<CR>" : ":wincmd j<CR>"
imap <silent> <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <silent> <expr> <C-l> pumvisible() ? "<C-y>" : "\<C-l>"

" Config edit/reload
noremap <silent> <F12> :sp ~/.config/nvim/init.vim<CR>
noremap <silent> <F24> :so ~/.config/nvim/init.vim<CR>

" H/L goto start/end of line
map H ^
map L $

noremap ; :


" Editor config
let mapleader = "\<Space>"
filetype plugin indent on
syntax on
set autoindent
set foldmethod=syntax

colors base16-tomorrow-night

set splitright
set splitbelow

set wildmenu
set wildmode=list:longest

set incsearch
set ignorecase
set smartcase
set gdefault
set shortmess+=sc

set ruler
set number
set relativenumber

noremap <leader>p :r !xclip -select clipboard -o<CR>
noremap <leader>y :w !xclip -select clipboard -i<CR><CR>
vnoremap <leader>y "+y
nnoremap <leader><leader> <C-^>
nnoremap <leader>l :vsp #<CR>
nnoremap <leader>j :sp #<CR>
nnoremap <leader>f za
nnoremap <leader>u zR
nnoremap <leader>n zM
nnoremap <leader>ca :%bd\|e#\|bd#<CR>
nmap <silent> <expr> <ESC> coc#util#has_float() ? ":call coc#util#float_hide()<CR>" : ":noh<CR>"

noremap <F1> <nop>

noremap <C-F> :Rg<space>
noremap <C-P> :Files<CR>
noremap <leader>; :Buffers<CR>

map <leader>bb :VBGtoggleBreakpointThisLine<CR>
map <leader>bl :VBGstepIn<CR>
map <leader>bj :VBGstepOver<CR>
map <leader>bh :VBGstepOut<CR>
map <leader>bB :VBGclearBreakpints<CR>

autocmd FileType python map <buffer> <F5> :VBGstartPDB %
autocmd FileType python map <buffer> <F10> :VBGcontinue

autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent! loadview

set updatetime=300
autocmd CursorHold * call CocActionAsync('highlight')

autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

let s:red_color="#bb0000"
let s:orange_color="#ba5d00"

exec "highlight CocErrorHighlight gui=undercurl guifg=" . s:red_color
exec "highlight CocErrorSign guifg=" . s:red_color
exec "highlight CocWarningHighlight gui=undercurl guifg=" . s:orange_color
exec "highlight CocWarningSign guifg=" . s:orange_color


" Use ripgrep instead of grep
if executable('rg')
	set grepprg=rg\ --no-heading\ --vimgrep
	set grepformat=%f:%l:%c:%m
endif


