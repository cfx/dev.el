call plug#begin()
"Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'elixir-editors/vim-elixir'
Plug 'overcache/NeoSolarized'
"Plug 'mhinz/vim-mix-format'
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'
Plug 'tpope/vim-fugitive'
Plug 'tomlion/vim-solidity'

call plug#end()
syntax on
set expandtab
set clipboard=unnamedplus
set autoread
set hid
set encoding=utf8
set smarttab
set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines
set nu

set nobackup
set nowb
set noswapfile
set laststatus=2
set statusline+=%F\ %l\:%c
set nohlsearch

let g:mix_format_on_save = 1

filetype plugin on
filetype indent on

autocmd FileType ruby setlocal shiftwidth=2 tabstop=2
autocmd FileType solidity setlocal shiftwidth=4 tabstop=4
autocmd FileType go nmap <leader>t  <Plug>(go-test)
autocmd FileType go nmap <leader>b  <Plug>(go-build)
autocmd FileType go nmap <leader>r  <Plug>(go-run)
autocmd FileType go nmap <leader>d  <Plug>(go-def)
au CursorHold,CursorHoldI * checktime
au FileType go setlocal omnifunc=go#complete#GocodeComplete

map <c-x>b :Buffers<CR>
map <c-x>k :bd<CR>
map <c-x><c-f> :Files<CR>
map <C-n> :cnext<CR>
map <C-m> :cprevious<CR>
nnoremap <leader>f :Files<CR>
nnoremap <leader>a :cclose<CR>
nnoremap <leader>s :Ag<space>
nnoremap <Leader>e :e <C-R>=expand('%:p:h') . '/'<CR>
nnoremap <Leader>t :exe "silent !tt.sh " . expand('%:p') . ":" . line('.')<CR> :redraw!<CR>
:imap jk <Esc>
autocmd BufWritePre * %s/\s\+$//e

cnoremap <C-A>		<Home>
cnoremap <C-B>		<Left>
cnoremap <C-D>		<Del>
cnoremap <C-E>		<End>
cnoremap <C-F>		<Right>
cnoremap <C-N>		<Down>
cnoremap <C-P>		<Up>
cnoremap <Esc><C-B>	<S-Left>
cnoremap <Esc><C-F>	<S-Right>

set background=light
"set t_Co=256
colorscheme NeoSolarized

"set termguicolors
"highlight Comment cterm=italic

:command Tt :exe "silent !tt.sh " . expand('%:p') | :redraw!
:command Ghl :exe "silent !ghl.sh " . expand('%:p') . " " . line('.')| :redraw!

nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> gi <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> K <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> <C-n> <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>
nnoremap <silent> <C-p> <cmd>lua vim.lsp.diagnostic.goto_next()<CR>

autocmd BufWritePre *.ex lua vim.lsp.buf.formatting_sync(nil, 100)
autocmd BufWritePre *.exs lua vim.lsp.buf.formatting_sync(nil, 100)

luafile ~/.config/nvim/compe-config.lua
luafile ~/.config/nvim/elixir-lsp.lua

