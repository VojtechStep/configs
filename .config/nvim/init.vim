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
let g:sandwich_no_default_key_mappings = 1
let g:operator_sandwich_no_default_key_mappings = 1
Plug 'machakann/vim-sandwich'

" Commenting
Plug 'tpope/vim-commentary'

" Count different casings as words
Plug 'chaoren/vim-wordmotion'

" Text alignment
Plug 'godlygeek/tabular'

" UNIX helpers
Plug 'tpope/vim-eunuch'

" Complete paired characters: (,[,{,< etc
let g:delimitMate_expand_cr = 2
let g:delimitMate_expand_space = 1
" let g:delimitMate_smart_quotes = "\w\%#"
" let g:delimitMate_matchpairs = "(:),{:},[:],<:>"
" let g:delimitMate_quotes = "\" ' `"
Plug 'Raimondi/delimitMate'

" Save as root
Plug 'lambdalisue/suda.vim'
command! W :w suda://%

" Navigation plugins
" ==============================================================================
"
" Automatically set project folder based on files in directory
let g:rooter_change_directory_for_non_project_files = 'current'
let g:rooter_patterns = ['Cargo.toml', 'package.json', 'pom.xml', 'stack.yaml', 'cabal.project', '.git/', '.vimproject']
let g:rooter_silent_chdir = 1
Plug 'airblade/vim-rooter'

" Fuzzy search in project files, open files
Plug '/usr/share/vim/vimfiles' " Managed via pacman
Plug 'junegunn/fzf.vim'

" Languages
" ==============================================================================
"
" LSP interface
Plug 'neoclide/coc.nvim', { 'do': 'yarn install --frozen-lockfile' }
Plug 'jackguo380/vim-lsp-cxx-highlight', { 'for': ['c', 'cpp'] }

" Plug 'neovim/nvim-lsp'

" Fish shell
Plug 'dag/vim-fish', { 'for': 'fish' }

" GraphQL schema
Plug 'jparise/vim-graphql', { 'for': 'graphqls' }

" XML Editing nicities
Plug 'sukima/xmledit', { 'for': 'xml' }

" Haskell
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'parsonsmatt/intero-neovim', { 'for': 'haskell' }

" PlantUML
let g:plantuml_set_makeprg = 0
Plug 'aklt/plantuml-syntax'

" TOML
Plug 'cespare/vim-toml'

" Nim
Plug 'zah/nim.vim'

" Crystal
let g:crystal_auto_format = 1
Plug 'rhysd/vim-crystal'

" Zig
Plug 'ziglang/zig.vim'

" D
Plug 'JesseKPhillips/d.vim'

" Markdown
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install' }

" Misc plugins
" ==============================================================================
"
" Base16 color schemes
Plug 'chriskempson/base16-vim'

" WakaTime time tracking
if executable("wakatime")
  let g:wakatime_OverrideCommandPrefix = exepath("wakatime")
endif
Plug 'wakatime/vim-wakatime'

" Pretty status line
" Plug 'vim-airline/vim-airline'
" Plug 'vim-airline/vim-airline-themes'

Plug 'itchyny/lightline.vim'

Plug 'jreybert/vimagit'

let g:org_agenda_files = ["~/org/index.org"]
let g:org_indent = 1
Plug 'jceb/vim-orgmode'

Plug 'inkarkat/vim-SyntaxRange'

Plug 'Shougo/vinarise.vim'

call plug#end()

runtime macros/sandwich/keymap/surround.vim

" Keybindings
" ==============================================================================
"
" Set space as Leader key
let mapleader = "\<Space>"

" Insert mode autocomplete
inoremap <silent> <expr> <C-Space> coc#refresh()
inoremap <silent> <C-d> <C-O>:call CocActionAsync('showSignatureHelp')<CR>

" LSP keybindings
"
" Symbol information
nmap <silent> gh :call CocActionAsync('doHover')<CR>

nmap <silent> <leader>h :call CocActionAsync('highlight')<CR>

" Error/Warning information
nmap <silent> <leader>e <Plug>(coc-diagnostic-info)

" Jump to definition
nmap <silent> <leader>d <Plug>(coc-definition)

" Jump to implementation
nmap <silent> <leader>i <Plug>(coc-implementation)

" Jump to references
nmap <silent> <leader>r <Plug>(coc-references)

" Rename symbol
nmap <silent> <F2> <Plug>(coc-rename)

" Format document
nmap <silent> <A-F> <Plug>(coc-format)

" Format selection
vmap <silent> <A-F> <Plug>(coc-format-selected)

" Show codelens actions
nmap <silent> <leader>c <Plug>(coc-codelens-action)

" Do quickfix
nmap <silent> <leader>q :CocAction<CR>

" Jump to next warning/error
nmap <silent> <F8> <Plug>(coc-diagnostic-next)

" Jump to previous warning/error
nmap <silent> <F20> <Plug>(coc-diagnostic-prev)

nmap <silent> <leader>sd :CocList diagnostics<CR>

function! s:show_documentation()
  if &filetype == 'vim' || &filetype == 'help'
    execute 'h '.expand('<cword>')
  else
    call CocActionAsync('doHover')
    " call LspHover
  endif
endfunction
nnoremap <silent> K :call <SID>show_documentation()<CR>

" Use Ctrl+arrow keys for split resizing
noremap <C-Right> <C-W>>
noremap <C-Left> <C-W><
noremap <C-Down> <C-W>+
noremap <C-Up> <C-W>-

" Scroll if has popup, jump othewise
nmap <silent> <expr> <C-k> coc#util#has_float() && coc#util#float_scrollable() ? ":call coc#util#float_scroll(0)<CR>" : ":wincmd W<CR>"

imap <silent> <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"
nmap <silent> <expr> <C-j> coc#util#has_float() && coc#util#float_scrollable() ? ":call coc#util#float_scroll(1)<CR>" : ":wincmd w<CR>"
imap <silent> <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <silent> <expr> <C-l> pumvisible() ? "<C-y>" : "\<C-l>"

" Switch to last buffer
nnoremap <silent> <leader><leader> <C-^>
" Split right
nnoremap <silent> <leader>l :vsp<CR>
" Split down
nnoremap <silent> <leader>j :sp<CR>

" Search in files, respect .gitignore
noremap <C-F> :Rg<space>
" Search in file names, respect .gitignore
noremap <silent> <expr> <leader>f len(system('git rev-parse')) ? ":Files\<CR>" : ":GFiles --exclude-standard --others --cached\<CR>"
" Search in open file names
noremap <silent> <leader>g :Buffers<CR>
" Search in changed files
" noremap <silent> <leader>g :GFiles?<CR>
" Search in help files
noremap <silent> <leader>sh :Helptags<CR>

function! s:setup_netrw_bindings()
  setl bufhidden=wipe
  silent! unmap <buffer> q
  map <silent> <buffer> q :bw<CR>
endfunction
augroup netrw_keybindings
  autocmd!
  autocmd FileType netrw :call <SID>setup_netrw_bindings()
augroup END

function! s:format_haskell()
  let l:pos = getpos('.')
  let l:tmp = tempname()
  silent execute 'w !brittany > '.l:tmp
  if v:shell_error == 0
    silent execute 'w !diff -q '.l:tmp.' -'
    if v:shell_error == 1
        silent execute '%!cat '.l:tmp
        call cursor(l:pos[1], l:pos[2])
    endif
  endif
  call delete(l:tmp)
endfunction

function! s:setup_haskell_bindings()
  nnoremap <silent> <leader>d :InteroGoToDef<CR>
  nnoremap <silent> <leader>h :InteroInfo<CR>
  nnoremap <silent> <leader>r :InteroLoadCurrentFile<CR>
  nnoremap <silent> <leader>o :InteroOpen<CR>
  nnoremap <silent> <leader>h :InteroHide<CR>
  nnoremap <silent> <A-F> :call <SID>format_haskell()<CR>
endfunction

augroup intero_keybingins
  autocmd!
  autocmd FileType haskell,hs :call <SID>setup_haskell_bindings()
  autocmd BufWritePost *.hs InteroReload
  autocmd BufWritePre *.hs :call <SID>format_haskell()
augroup END

function! s:setup_help_bindings()
  silent! unmap <buffer> q
  map <silent> <buffer> q :bw<CR>
endfunction
augroup help_bindings
  autocmd!
  autocmd FileType help,qf :call <SID>setup_help_bindings()
augroup END

function! s:setup_scheme()
  let g:delimitMate_expand_cr = 0
  let g:delimitMate_quotes = "\""
endfunction
augroup scheme_env
  autocmd!
  autocmd FileType scheme :call <SID>setup_scheme()
augroup END

function! s:setup_nasm()
endfunction

augroup nasm
  autocmd!
  autocmd BufRead,BufNewFile *.asm,*.nasm set ft=nasm
  autocmd FileType nasm :call <SID>setup_nasm()
augroup END

function! s:setup_plantuml()
  let &l:makeprg = 'sh -c "docker run -i --rm think/plantuml:latest < % > ' . expand('%:r') . '.svg"'
endfunction

augroup plantuml
  autocmd!
  autocmd FileType plantuml :call <SID>setup_plantuml()
augroup END

" Close others
nnoremap <silent> <leader>ca :%bd<bar>e#<bar>bd#<CR>
" Close buffer
nnoremap <silent> <leader>cc :bw<CR>
" Hide popup or stop highlight
nmap <silent> <expr> <ESC> coc#util#has_float() ? ":call coc#util#float_hide()<CR>" : ":noh<CR>"

" Open terminal to the right
map <silent> <leader><BS> :vsp term://fish<CR>

" Jump between splits from terminal
tmap <silent> <C-H> <C-\><C-N>:wincmd h<CR>
tmap <silent> <C-L> <C-\><C-N>:wincmd l<CR>
tmap <silent> <C-f> <C-\><C-N>

" Don't show help with <F1>, i know my setup
noremap <F1> <nop>

" Sometimes you press q to exit from a buffer and when it doesn't work, you
" try to do :q and bad things happen
" nnoremap q: :

" Config edit/reload
noremap <silent> <F12> :sp ~/.config/nvim/init.vim<CR>
noremap <silent> <F24> :so ~/.config/nvim/init.vim<CR>

" H/L goto start/end of line
map H ^
map L $
imap <C-h> <C-o>H
imap <C-l> <C-o>L

noremap ; :

" Operate on system clipboard
noremap <silent> <leader>p "+p
vnoremap <silent> <leader>p "+p
noremap <silent> <leader>P "+P
vnoremap <silent> <leader>P "+P
noremap <silent> <leader>y "+y
vnoremap <silent> <leader>y "+y

" Toggle fold
" nnoremap <silent> <leader>f za
" Unfold all
" nnoremap <silent> <leader>u zR
" Fold all
" nnoremap <silent> <leader>n zM


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
" set foldmethod=syntax
" set foldminlines=10

" Show cursor position in lower right corner
set ruler
" Show line number on current line...
set number
" ... and show relative line number on other lines
set relativenumber

" Keep a 3 line margin between the cursor and the edge of the screen
set scrolloff=3

" Use two spaces as tabs
set tabstop=2
set shiftwidth=2
set expandtab

augroup filetype_options
  autocmd!
  autocmd FileType c,cpp set tabstop=2
  autocmd FileType c,cpp set shiftwidth=2
augroup END

" Disable netrw history
let g:netrw_dirhistmax = 0
" Set default netrw view to Tree
let g:netrw_liststyle = 3

" Open previews to the right
let g:netrw_preview = 1
let g:netrw_alto = 0

" Don't wrap text
set nowrap

" Hide concealed text if the cursor is not on the same line (eg Markdown links)
set conceallevel=2

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
  " autocmd CursorHold * call CocActionAsync('highlight')
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

command! PreviewMarkdown MarkdownPreview

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
let s:blue_color = "#005dbb"
let s:ligh_blue_color = "#15aabf"
exec "highlight CocErrorHighlight gui=undercurl,bold guisp=" . s:red_color
exec "highlight CocErrorSign guifg=" . s:red_color
exec "highlight CocWarningHighlight gui=undercurl guisp=" . s:orange_color
exec "highlight CocWarningSign guifg=" . s:orange_color
exec "highlight CocInfoHighlight gui=undercurl guisp=" . s:blue_color
exec "highlight CocInfoSign guifg=" . s:blue_color
exec "highlight CocHintHighlight gui=undercurl guisp=" . s:ligh_blue_color
exec "highlight CocHintSign guifg=" . s:ligh_blue_color

" Highlight trailing whitespace as error
exec "highlight ExtraWhitespace guibg=" . s:red_color
match ExtraWhitespace /\s\+$/

