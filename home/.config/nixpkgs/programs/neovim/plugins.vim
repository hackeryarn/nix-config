" Theme
let g:airline_theme='solarized'

" shows list of yanked text (coc-yank plugin)
nnoremap <silent> <space>y  :<C-u>CocList -A --normal yank<cr>

" EasyMotion search with highlighting
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Fuzzy finder shortcut
nnoremap <space>f :FZF<CR>
nnoremap <space>/ :Rg<CR>
nnoremap <space>b :Buffer<CR>

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

" Ale configs
let g:ale_linters = {
      \ 'python': ['flake8', 'pyls'],
      \ 'clojure': ['clj-knodo'],
      \ 'scss': ['prettier'],
      \ 'html': ['prettier'],
      \ 'nix': ['rnix-lsp'],
      \ 'haskell': ['hls'],
      \ }
let g:ale_fixers = {
      \ '*': ['remove_trailing_lines',  'trim_whitespace'],
      \ 'python': ['autopep8', 'isort'],
      \ 'scss': ['prettier'],
      \ 'html': ['prettier'],
      \ 'haskell': ['brittany'],
      \ 'nix': ['nixfmt'],
      \ }
let g:ale_completion_autoimport = 1
let g:ale_fix_on_save = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
" set omnifunc=ale#completion#OmniFunc

nmap <silent> [d <Plug>(ale_go_to_definition)
nmap <silent> ]d <Plug>(ale_go_to_definition)
nmap <silent> [r <Plug>(ale_find_references)
nmap <silent> ]r <Plug>(ale_find_references)
nmap K <Plug>(ale_documentation)
nmap <silent> [W <Plug>(ale_first)
nmap <silent> [w <Plug>(ale_previous)
nmap <silent> ]w <Plug>(ale_next)
nmap <silent> ]W <Plug>(ale_last)
autocmd InsertLeave * silent! pclose!
inoremap <silent><expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
inoremap <silent><expr><s-tab> pumvisible() ? "\<c-p>" : "\<s-tab>"

" Deoplete
let g:deoplete#enable_at_startup = 1
call deoplete#custom#option('sources', {
      \ '_': ['ale', 'buffer'],
      \})
