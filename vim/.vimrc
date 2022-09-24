" Plugins
let g:netrw_dirhistmax = 0
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source ~/.vimrc
endif
call plug#begin()
Plug 'dense-analysis/ale'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'sheerun/vim-polyglot'
call plug#end()

" Flags
set autoindent
set backspace=indent,eol,start
set ignorecase
set incsearch
set nowrap
set number

" Styles
colorscheme default
highlight! link SignColumn LineNr| "See airblade/vim-gitgutter#696

" Netrw
let g:netrw_banner = 0
function! NetrwMapping()
  nmap <buffer> h -^
  nmap <buffer> l <CR>
endfunction
augroup netrw_mapping
  autocmd!
  autocmd filetype netrw call NetrwMapping()
augroup END

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
\   'ruby': ['rubocop'],
\   'rust': ['rustfmt'],
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

" Keymaps
nnoremap <leader>d :Explore<cr>
nnoremap <leader>f :FZF<cr>
nnoremap <leader>g :Rg<cr>
nnoremap gb :Git blame<cr>
nnoremap gd :ALEGoToDefinition<cr>
