" Enable hlint and GHC via Cabal
let g:ale_linters = {'haskell': ['hlint', 'cabal-build'], 'c': ['clangtidy', 'cc']}
" ... only
let g:ale_linters_explicit = 1
" Don't lint until I save
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_enter = 0

call ale#Set('haskell_cabal_build_options', '--enable-tests --disable-optimization --enable-benchmarks')
call ale#Set('c_cc_executable', 'gcc')
call ale#Set('c_cc_options', '-std=c99 -march=native -Wall -Wextra')

function! GetCabalCommand(buffer) abort
  let l:flags = ale#Var(a:buffer, 'haskell_cabal_build_options')
  return 'cabal new-build ' . l:flags
endfunction

call ale#linter#Define('haskell', {
      \ 'name': 'cabal_build',
      \ 'aliases': ['cabal-build'],
      \ 'output_stream': 'stderr',
      \ 'executable': 'cabal',
      \ 'command': function('GetCabalCommand'),
      \ 'callback': 'ale#handlers#haskell#HandleGHCFormat',
      \})

" Configure Neoformat to use cabal-fmt for Cabal files
let g:neoformat_cabal_cabalfmt = { 'exe': 'cabal-fmt', 'args': [] }
let g:neoformat_enabled_cabal = ['cabalfmt']

" Configure Neoformat to use ormolu for Haskell
let g:neoformat_haskell_ormolu = { 'exe': 'ormolu', 'args': [] }
let g:neoformat_enabled_haskell = ['ormolu']

" Configure Neoformat to use clang-format for C
let g:neoformat_enabled_c = ['clang-format']

" Enable automagic autoformatting
augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | Neoformat
augroup end
