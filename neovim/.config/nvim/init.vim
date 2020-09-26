" Nvim setup
let g:python3_host_prog = expand('~/.pyenv/versions/3.8.2/bin/python')
let g:python_host_prog = expand('~/.pyenv/versions/2.7.17/bin/python')

" Base config
syntax enable
let mapleader = " "
set backspace=2
set hidden
set nobackup
set nowritebackup
set noswapfile
set history=50
set ruler
set showcmd
set laststatus=2
set autowrite
set modelines=0
set nomodeline
set tabstop=2
set shiftwidth=2
set shiftround
set expandtab
set nojoinspaces
set textwidth=80
set colorcolumn=+1
set number
set numberwidth=5
set spell spelllang=en_us

" Search
set gdefault
set incsearch
set ignorecase
set smartcase
set inccommand=split
nnoremap ,, :nohlsearch<CR>

" keep undo files
set undofile
set undodir=~/.vimundo/
augroup vimrc
	autocmd!
  autocmd BufWritePre /tmp/* setlocal noundofile

  " Source the vimrc file after saving it
	autocmd BufWritePost init.vim source $MYVIMRC
augroup END

" netrw
let g:netrw_liststyle = 3 " list child directories

" Splits
set splitbelow
set splitright

" Folding
set foldmethod=syntax
set nofoldenable
noremap zA za
noremap za zA

" Quality of Life Mapping
imap fd <Esc>
nnoremap H ^
nnoremap L g_
nnoremap ; :
nnoremap <Leader>; ;
nnoremap <Leader>, ,
nmap <leader>v :tabedit $MYVIMRC<CR>

" Make command history easier to navigate
cnoremap <c-n> <down>
cnoremap <c-p> <up>

" Easy window motions
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Edit macro shortcut
nnoremap <leader>m  :<c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>

" Switch between the last two files
nnoremap <Leader><Leader> <C-^>

" Open file relative to the current file
cnoremap %% <C-R>=fnameescape(expand('%:h')).'/'<cr>
map <leader>ew :e %%
map <leader>es :sp %%
map <leader>ev :vsp %%
map <leader>et :tabe %%

" Select pasted text
:nnoremap gV `[v`]

" makes * and # work on visual mode too.
function! s:VSetSearch(cmdtype)
  let temp = @s
  norm! gv"sy
  let @/ = '\V' . substitute(escape(@s, a:cmdtype.'\'), '\n', '\\n', 'g')
  let @s = temp
endfunction

xnoremap * :<C-u>call <SID>VSetSearch('/')<CR>/<C-R>=@/<CR><CR>
xnoremap # :<C-u>call <SID>VSetSearch('?')<CR>?<C-R>=@/<CR><CR>

" Terminal
if has('nvim')
  tnoremap fd <C-\><C-n>
  highlight! link TermCursor Cursor
  highlight! TermCursorNC guibg=red guifg=white ctermbg=1 ctermfg=15
  tnoremap <C-h> <C-\><C-n><C-h>
  tnoremap <C-j> <C-\><C-n><C-j>
  tnoremap <C-k> <C-\><C-n><C-k>
  tnoremap <C-l> <C-\><C-n><C-l>
endif

if has('nvim') && executable('nvr')
  let $GIT_EDITOR="nvr -cc split --remote-wait"
endif

" Package management
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

call plug#begin()

" General
Plug 'godlygeek/tabular'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'mhinz/vim-grepper'
Plug 'sjl/gundo.vim'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'

" Themes
Plug 'altercation/vim-colors-solarized'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

"Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Dev
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'dense-analysis/ale'
Plug 'editorconfig/editorconfig-vim'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'vim-test/vim-test'

" Languages
Plug 'fatih/vim-go', { 'for': 'go' }
Plug 'masukomi/vim-markdown-folding', { 'for': 'markdown' }
Plug 'mattn/emmet-vim'
Plug 'otherjoel/vim-pollen'
Plug 'tpope/vim-fireplace', { 'tag': 'v2.1', 'for': 'clojure' }
Plug 'venantius/vim-cljfmt', { 'for': 'clojure' }
Plug 'wlangstroth/vim-racket', { 'for': 'racket' }

" Optional
Plug 'tpope/vim-scriptease', {'type': 'opt'}

call plug#end()

" Theme setup
set background=light
let g:solarized_termcolors=256
let g:solarized_termtrans=1
colorscheme solarized

" FZF
function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)
nnoremap <Leader>f :<C-u>Files<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>m :Maps<CR>
nnoremap gb <C-^>

" Dispatch
nnoremap <Leader>dl :Copen<NL>:Dispatch<CR>

" Fugitive
augroup fugitive
  autocmd FileType gitcommit,gitrebase,gitconfig set bufhidden=delete
  autocmd BufReadPost fugitive://* set bufhidden=delete
  autocmd User fugitive
    \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
    \   nnoremap <buffer> .. :edit %:h<CR> |
    \ endif
augroup END

" Grepper
let g:grepper = {}
let g:grepper.tools = ['rg', 'git', 'grep']
nnoremap <Leader>* :Grepper -cword -noprompt<CR>
nmap gs <Plug>(GrepperOperator)
xmap gs <Plug>(GrepperOperator)
nnoremap <Leader>g :Grepper -tool rg<CR>
nnoremap <Leader>G :RG<CR>

" Testing
let test#strategy = "dispatch"
let test#python#djangotest#options = '--no-input --keepdb'
nmap <silent> <leader>tn :TestNearest<CR>
nmap <silent> <Leader>tf :TestFile<CR>
nmap <silent> <Leader>ts :TestSuite<CR>
nmap <silent> <Leader>tl :TestLast<CR>
nmap <silent> <Leader>tg :TestVisit<CR>

" Projects
augroup configure_projects
  autocmd!
  autocmd User ProjectionistActivate call s:linters()
augroup END

function! s:linters() abort
  for [root, value] in projectionist#query('linters')
    let b:ale_linters = {&filetype: value}
    break
  endfor
endfunction

" Ale
let g:ale_fixers = { '*': ['remove_trailing_lines', 'trim_whitespace'] }
let g:ale_fix_on_save = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
nmap <silent> [d <Plug>(ale_go_to_definition)
nmap <silent> ]d <Plug>(ale_go_to_definition)
nmap <silent> [r <Plug>(ale_find_references)
nmap <silent> ]r <Plug>(ale_find_references)
nmap K <Plug>(ale_documentation)
nmap <silent> [W <Plug>(ale_first)
nmap <silent> [w <Plug>(ale_previous)
nmap <silent> ]w <Plug>(ale_next)
nmap <silent> ]W <Plug>(ale_last)

" Completion
set omnifunc=ale#completion#OmniFunc
let g:deoplete#enable_at_startup = 1
autocmd InsertLeave * silent! pclose!
inoremap <silent><expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
inoremap <silent><expr><s-tab> pumvisible() ? "\<c-p>" : "\<s-tab>"
let g:UltiSnipsExpandTrigger = "<c-e>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" Clojure
command! JackIn :Piggieback (figwheel-sidecar.repl-api/repl-env)

augroup pollen
    autocmd!

    " Set Pollen syntax for files with these extensions:
    au! BufRead,BufNewFile *.pm set filetype=pollen
    au! BufRead,BufNewFile *.pp set filetype=pollen
    au! BufRead,BufNewFile *.ptree set filetype=pollen

    " Editor settings:
    autocmd FileType pollen setlocal formatoptions+=t
    autocmd FileType pollen setlocal wrap
    autocmd FileType pollen setlocal linebreak
    autocmd FileType pollen setlocal textwidth=0
    autocmd FileType pollen setlocal wrapmargin=0
    autocmd FileType pollen setlocal showbreak=…
augroup END
