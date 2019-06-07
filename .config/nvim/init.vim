" Use sh if fish is the default shell
if &shell =~# 'fish$'
	set shell=sh
endif

" Use ripgrep instead of grep
if executable('rg')
	set grepprg=rg\ --no-heading\ --vimgrep
	set grepformat=%f:%l:%c:%m
endif

call plug#begin('~/.local/share/nvim/plugged')


" Editing plugins
" ==============================================================================
"
" Surrounding
Plug 'machakann/vim-sandwich'

" Commenting
Plug 'tpope/vim-commentary'

"	Git changes
Plug 'airblade/vim-gitgutter'

" Undo branch visualizer
Plug 'mbbill/undotree'
let g:undotree_SetFocusWhenToggle = 1

"	Distractionless editing
" Plug 'junegunn/goyo.vim'
" Plug 'junegunn/limelight.vim'
" let g:goyo_height = '90%'
" let g:goyo_width = 100

"	Count different casings as words
Plug 'chaoren/vim-wordmotion'

"	Japanese input
" Plug 'tyru/eskk.vim'

"	Complete paired characters: (,[,{,< etc
Plug 'Raimondi/delimitMate'
let delimitMate_expand_cr = 1

" Save as root
Plug 'lambdalisue/suda.vim'
command! W :w suda://%


" Navigation plugins
" ==============================================================================
"
" Automatically set project folder based on files in directory
Plug 'airblade/vim-rooter'
let g:rooter_change_directory_for_non_project_files = 'current'
let g:rooter_patterns = ['.git/', 'Cargo.toml', 'package.json', 'pom.xml']
let g:rooter_silent_chdir = 1

"	Fuzzy search in project files, open files
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'


" Languages
" ==============================================================================
"
" LSP interface
Plug 'neoclide/coc.nvim', { 'do': 'yarn install --frozen-lockfile' }

" Language plugins bundle
Plug 'sheerun/vim-polyglot'

" GraphQL schema
Plug 'jparise/vim-graphql', { 'for': 'graphqls' }

" Flutter
Plug 'thosakwe/vim-flutter', { 'for': 'dart' }

" XML Editing nicities
Plug 'sukima/xmledit', { 'for': 'xml' }


" Misc plugins
" ==============================================================================
"
" Base16 color schemes
Plug 'chriskempson/base16-vim'

" WakaTime time tracking
Plug 'wakatime/vim-wakatime'

call plug#end()


" Keybindings
" ==============================================================================
"
" Set space as Leader key
let mapleader = "\<Space>"

" Insert mode autocomplete
inoremap <silent> <expr> <C-Space> coc#refresh()
inoremap <silent> <C-d> <C-O>:call CocActionAsync('showSignatureHelp')<CR>

" LSP keybindings
" Symbol information
nmap <silent> gh :call CocActionAsync('doHover')<CR>
" Error/Warning information
nmap <silent> <leader>e <Plug>(coc-diagnostic-info)
" Jump to definition
nmap <silent> <leader>d <Plug>(coc-definition)
" Jump to references
nmap <silent> <leader>r <Plug>(coc-references)
" Rename symbol
nmap <silent> <F2> <Plug>(coc-rename)
" Format document
nmap <silent> <A-F> <Plug>(coc-format)
" Format selection
vmap <silent> <A-F> <Plug>(coc-format-selected)
" Show code actions
nmap <silent> <Tab> :CocAction<CR>
" Show codelens actions
nmap <silent> <leader>c <Plug>(coc-codelens-action)
" Do quickfix
nmap <silent> <leader>q <Plug>(coc-fix-current)
" Jump to next warning/error
nmap <silent> <F8> <Plug>(coc-diagnostic-next)
" Jump to previous warning/error
nmap <silent> <F20> <Plug>(coc-diagnostic-prev)

function! s:show_documentation()
  if &filetype == 'vim' || &filetype == 'help'
    execute 'h '.expand('<cword>')
  else
    call CocActionAsync('doHover')
  endif
endfunction
nnoremap <silent> K :call <SID>show_documentation()<CR>

" Jump between splits
nmap <silent> <C-l> :wincmd l<CR>
nmap <silent> <C-h> :wincmd h<CR>

" Use Ctrl+arrow keys for split resizing
noremap <C-Right> <C-W>>
noremap <C-Left> <C-W><
noremap <C-Down> <C-W>+
noremap <C-Up> <C-W>-

" Scroll if has popup, jump othewise
nmap <silent> <expr> <C-k> coc#util#has_float() && coc#util#float_scrollable() ? ":call coc#util#float_scroll(0)<CR>" : ":wincmd k<CR>"
imap <silent> <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"
nmap <silent> <expr> <C-j> coc#util#has_float() && coc#util#float_scrollable() ? ":call coc#util#float_scroll(1)<CR>" : ":wincmd j<CR>"
imap <silent> <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <silent> <expr> <C-l> pumvisible() ? "<C-y>" : "\<C-l>"

" Switch to last buffer
nnoremap <leader><leader> <C-^>
" Split right
nnoremap <leader>l :vsp<CR>
" Split down
nnoremap <leader>j :sp<CR>

" Search in files, respect .gitignore
noremap <C-F> :Rg<space>
" Search in file names, respect .gitignore
noremap <expr> <C-P> len(system('git rev-parse')) ? ":Files\<CR>" : ":GFiles --exclude-standard --others --cached\<CR>"
" Search in open file names
noremap <leader>b :Buffers<CR>
" Search in changed files
noremap <leader>g :GFiles?<CR>
" Search in help files
noremap <leader>hh :Helptags<CR>

" Show undo graph
noremap <leader>v :UndotreeShow<CR>

function! s:setup_undotree_bindings()
	" Go to last change
	nmap <buffer> U ggjj<CR>
	" Close window
	nmap <buffer> <ESC> q
	" Bad things happen when you do :q
	nmap <buffer> :q q
endfunction

augroup undotree_keybindings
	autocmd!
	autocmd FileType undotree :call <SID>setup_undotree_bindings()
augroup END

function! s:setup_netrw_bindings()
	unmap <buffer> q
	nmap <buffer> q :bw<CR>
	nmap <buffer> <ESC> q
	nmap <buffer> :q q
endfunction

augroup netrw_keybindings
	autocmd!
	autocmd FileType netrw :call <SID>setup_netrw_bindings()
augroup END

" Close others
nnoremap <leader>ca :%bd<bar>e#<bar>bd#<CR>
" Close buffer
nnoremap <leader>cc :bw<CR>
" Hide popup or stop highlight
nmap <silent> <expr> <ESC> coc#util#has_float() ? ":call coc#util#float_hide()<CR>" : ":noh<CR>"

" ESC only closes popup in insert mode, press again to enter normal mode
" (Still undecided on which behaviour I want)
"
" imap <silent> <expr> <ESC> pumvisible() ? "<C-O>:call coc#util#float_hide()<CR>" : "<ESC>"

" Open terminal to the right
map <leader><BS> :vsp term://fish<CR>

" Jump between splits from terminal
tmap <C-H> <C-\><C-N>:wincmd h<CR>
tmap <C-L> <C-\><C-N>:wincmd l<CR>

" Don't show help with <F1>, i know my setup
noremap <F1> <nop>

" Sometimes you press q to exit from a buffer and when it doesn't work, you
" try to do :q and bad things happen
nnoremap q: :

" Config edit/reload
noremap <silent> <F12> :sp ~/.config/nvim/init.vim<CR>
noremap <silent> <F24> :so ~/.config/nvim/init.vim<CR>

" H/L goto start/end of line
map H ^
map L $

noremap ; :

" Operate on system clipboard
noremap <leader>p :r !xclip -select clipboard -o<CR>
vnoremap <leader>p "+p
noremap <leader>y :w !xclip -select clipboard -i<CR><CR>
vnoremap <leader>y "+y

" Toggle fold
nnoremap <leader>f za
" Unfold all
nnoremap <leader>u zR
" Fold all
nnoremap <leader>n zM

" Toggle zen mode
nnoremap <leader>z :Goyo<CR>


" Editor config
" ==============================================================================
"
" Autodetect file type, load corresponding plugins, load indent file
filetype plugin indent on
" Enable syntax highlight
syntax on
" Copy indent from previous line
set autoindent
" Load folding rules from syntax files
set foldmethod=syntax

" Show cursor position in lower right corner
set ruler
" Show line number on current line...
set number
" ... and show relative line number on other lines
set relativenumber

" Enable mouse in normal and insert mode
" It's only for the terminal, I swear
set mouse=ni

" Render tabs as 2 spaces, but still use tabs
set tabstop=2
set shiftwidth=2
set noexpandtab

" ... Except for Haskell. Use spaces in Haskell. Because Haskell
augroup filetype_options
	autocmd!
	autocmd FileType haskell,fsharp set expandtab
augroup END

" Disable netrw history
let g:netrw_dirhistmax = 0
" Set default netrw view to Tree
let g:netrw_liststyle = 3

" Don't wrap text
set nowrap

" Hide concealed text if the cursor is not on the same line (eg Markdown links)
set conceallevel=2

augroup goyo_limelight
	autocmd!
	autocmd User GoyoEnter Limelight
	autocmd User GoyoLeave Limelight!
augroup END

" Use abbreviated status and silent search, don't show messages about search
" wrapping around file
set shortmess+=acs
set noshowmode

" Enable autocomplete in commands
set wildmenu
" First insert longest common part, then show list of possible completions
set wildmode=longest,list

" Save undo history for all files in a directory
set undodir=~/.vimundo
set undofile

" Only hide closing buffers
set hidden

" Use python 3 where aplicable
set pyxversion=3

" Open splits to right/down by default
set splitright
set splitbelow

" Preview first match
set incsearch
" Ignore case in search...
set ignorecase
" ... but only if the search expression is all lowercase
set smartcase
" Enable 'g' flag by default (for substitute command and such)
set gdefault

" Set View as a group of cursor position, working directory and fold states
set viewoptions=cursor,curdir,folds
" Save views on exit, load views on open
augroup view_mgmt
	autocmd!
	autocmd BufWinLeave *.* mkview
	autocmd BufWinEnter *.* silent! loadview
augroup END

" Wait 300ms before highlighting words
set updatetime=300
augroup coc_autocmd
	autocmd!
	" Highlight instances of symbol under cursor after stoping on it
	autocmd CursorHold * call CocActionAsync('highlight')
	" Show function signature when jumping around placeholders
	autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup END

" Write to path that is yet to exist
function! s:WriteCreatingDirs()
	execute ':silent !mkdir -p %:h'
	write
endfunction
command! WW call <SID>WriteCreatingDirs()

" Edit relative to open file
" TODO: Implement autocomplete
command! -nargs=? E :e %:h/<args>

" Color config
" ==============================================================================
"
" Enable support for RGB colors in terminal
set termguicolors

" Use Tomorrow night color scheme
colors base16-tomorrow-night

" Symbol occurences highlight
highlight CocHighlightText guibg=#373b61

" Error and warning styles: undercurl with colored background
let s:red_color = "#bb0000"
let s:orange_color = "#ba5d00"
exec "highlight CocErrorHighlight gui=undercurl guibg=" . s:red_color
exec "highlight CocErrorSign guifg=" . s:red_color
exec "highlight CocWarningHighlight gui=undercurl guibg=" . s:orange_color
exec "highlight CocWarningSign guifg=" . s:orange_color

" Highlight trailing whitespace as error
exec "highlight ExtraWhitespace guibg=" . s:red_color
match ExtraWhitespace /\s\+$/

