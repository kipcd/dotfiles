set nocompatible            " disable compatibility to old-time vi
set showmatch               " show matching brackets.
set ignorecase              " case insensitive matching
set hlsearch                " highlight search results
set tabstop=4               " number of columns occupied by a tab character
set softtabstop=4           " see multiple spaces as tabstops so <BS> does the right thing
set expandtab               " converts tabs to white space
set shiftwidth=4            " width for autoindents
set autoindent              " indent a new line the same amount as the line just typed
set number                  " add line numbers
set wildmode=longest,list   " get bash-like tab completions
set colorcolumn=80          " set an 80 column border for good coding style
filetype plugin indent on   " allows auto-indenting depending on file type
syntax on                   " syntax highlighting
colorscheme torte           " set one of predefined colorscheme
set clipboard+=unnamedplus  " use system clipboard
set splitbelow              " open new split below
set splitright              " open new split right

" Map russian keys in normal mode
set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz

if has('nvim')
    tnoremap <Esc> <C-\><C-n>   " switch from terminal mode to normal mode
    tnoremap <A-[> <Esc>        " remap Esc for terminal
    tnoremap <M-t> <C-\><C-n> :split +terminal<CR>
    tnoremap <A-S-t> <C-\><C-n> :vsplit +terminal<CR>

    " Navigate between splits
    "
    " Terminal mode:
    tnoremap <A-h> <c-\><c-n><c-w>hi
    tnoremap <A-j> <c-\><c-n><c-w>ji
    tnoremap <A-k> <c-\><c-n><c-w>ki
    tnoremap <A-l> <c-\><c-n><c-w>li
    tnoremap <A-w> <c-\><c-n><c-w>wi
    " Insert mode:
    inoremap <A-h> <Esc><c-w>h
    inoremap <A-j> <Esc><c-w>j
    inoremap <A-k> <Esc><c-w>k
    inoremap <A-l> <Esc><c-w>l
    inoremap <A-w> <Esc><c-w>w
    " Visual mode:
    vnoremap <A-h> <Esc><c-w>h
    vnoremap <A-j> <Esc><c-w>j
    vnoremap <A-k> <Esc><c-w>k
    vnoremap <A-l> <Esc><c-w>l
    vnoremap <A-w> <Esc><c-w>w
    " Normal mode:
    nnoremap <A-h> <c-w>h
    nnoremap <A-j> <c-w>j
    nnoremap <A-k> <c-w>k
    nnoremap <A-l> <c-w>l
    nnoremap <A-w> <c-w>w
endif

" Open terminal in Insert mode by default
if has('nvim')
    autocmd TermOpen term://* startinsert
endif

