"Plugins{{{
call plug#begin()
Plug 'nfnty/vim-nftables'       " syntax for nftables
Plug 'wadackel/vim-dogrun'      " nvim dogrun theme
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'nvim-orgmode/orgmode'     " emacs orgmode
" Git integration
Plug 'mhinz/vim-signify'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'junegunn/gv.vim'

call plug#end()
"}}}
"SET{{{
colorscheme dogrun
set foldmethod=marker
set clipboard+=unnamedplus
set number
set relativenumber
syntax on
set nocompatible
filetype on
filetype plugin on
filetype indent on
set shiftwidth=4
set tabstop=4
set expandtab
set nobackup
set scrolloff=10
set nowrap
set incsearch
set ignorecase
set smartcase
set showcmd
set showmode
set showmatch
set hlsearch
"}}}
"KEYMAPS {{{
"imap jj <Esc>
"imap jk <Esc>
"imap kk <Esc>:w<Enter>a

"COlEMAK {{{
" colemak
"imap tn <ESC>
"vmap tn <ESC>
"
"noremap n j
"noremap N J
"noremap e k
"noremap E K
"
"noremap k n
"noremap K N
"
"noremap j e
"noremap J E
"}}}
"}}}
" Automatically deletes all trailing whitespace and newlines at end of file on save. & reset cursor position {{{
 	autocmd BufWritePre * let currPos = getpos(".")
	autocmd BufWritePre * %s/\s\+$//e
	autocmd BufWritePre * %s/\n\+\%$//e
	autocmd BufWritePre *.[ch] %s/\%$/\r/e
  	autocmd BufWritePre * cal cursor(currPos[1], currPos[2])
"}}}
" Enable autocompletion: {{{
	set wildmode=longest,list,full
"}}}
" Disables automatic commenting on newline: {{{
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
"}}}
" For org mode {{{
" https://github.com/nvim-orgmode/orgmode
lua << EOF

-- Load custom tree-sitter grammar for org filetype
require('orgmode').setup_ts_grammar()

-- Tree-sitter configuration
require'nvim-treesitter.configs'.setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop, highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = {'org'}, -- Required for spellcheck, some LaTex highlights and code block highlights that do not have ts grammar
  },
  ensure_installed = {'org'}, -- Or run :TSUpdate org
}

require('orgmode').setup({
  -- org_agenda_files = {'~/Dropbox/org/*', '~/my-orgs/**/*'},
  org_default_notes_file = '~/org/**',
})
EOF
"}}}

source $HOME/.config/nvim/plug-config/signify.vim
