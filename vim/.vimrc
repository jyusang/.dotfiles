" Plugin
let g:netrw_dirhistmax = 0
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source ~/.vimrc
endif
call plug#begin()
Plug 'Yggdroot/indentLine'
Plug 'airblade/vim-gitgutter'
Plug 'chriskempson/base16-vim'
Plug 'embear/vim-localvimrc'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-fugitive'
Plug 'w0rp/ale'
call plug#end()

" Behavior
set autoread
set lazyredraw
set mouse=a
set updatetime=200
set visualbell

" Display
set number
set wildmenu
set regexpengine=2

" Format
filetype plugin indent on
set encoding=utf-8
syntax enable

" Indentation
set autoindent
set backspace=indent,eol,start
set expandtab
set shiftwidth=2
set softtabstop=2

" Search
set ignorecase
set incsearch
set nohlsearch
set smartcase

" Style
let &t_8f = "\<Esc>[38:2:%lu:%lu:%lum"
let &t_8b = "\<Esc>[48:2:%lu:%lu:%lum"
set termguicolors
if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
  " Hotfix spell highlight: https://github.com/chriskempson/base16-vim/issues/182
  call Base16hi("SpellBad",   "", "", g:base16_cterm08, g:base16_cterm00, "", "")
  call Base16hi("SpellCap",   "", "", g:base16_cterm0A, g:base16_cterm00, "", "")
  call Base16hi("SpellLocal", "", "", g:base16_cterm0D, g:base16_cterm00, "", "")
  call Base16hi("SpellRare",  "", "", g:base16_cterm0B, g:base16_cterm00, "", "")
endif

" Key maps
nnoremap gd :ALEGoToDefinition<CR>

" ALE
set omnifunc=ale#completion#OmniFunc
let g:ale_fix_on_save = 1
let g:ale_fixers = {
\   'c': ['clang-format'],
\   'cpp': ['clang-format'],
\   'css': ['prettier'],
\   'dart': ['dart-format'],
\   'go': ['gofmt'],
\   'html': ['prettier'],
\   'javascript': ['prettier'],
\   'javascriptreact': ['prettier'],
\   'json': ['prettier'],
\   'kotlin': ['ktlint'],
\   'less': ['prettier'],
\   'markdown': ['prettier'],
\   'python': ['black'],
\   'rust': ['rustfmt'],
\   'ruby': ['rubocop'],
\   'scss': ['prettier'],
\   'sh': ['shfmt'],
\   'typescript': ['prettier'],
\   'typescriptreact': ['prettier'],
\}
let g:ale_linters = {
\   'dart': ['dart_analyze'],
\   'go': ['gopls'],
\   'javascript': ['eslint', 'tsserver'],
\   'javascriptreact': ['eslint', 'tsserver'],
\   'python': ['pyright'],
\   'rust': ['rls'],
\}
let g:ale_linter_aliases = {
\   'typescriptreact': 'typescript'
\}
let g:indentLine_defaultGroup = 'NonText'
let g:indentLine_enabled = 0
