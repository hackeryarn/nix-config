set tabstop=4
set softtabstop=4
set shiftwidth=4
set textwidth=79
set expandtab
set autoindent
set fileformat=unix

let b:ale_linters = ['flake8', 'pyls']
let b:ale_fixers = ['autopep8', 'isort', 'remove_trailing_lines',
      \ 'trim_whitespace']
