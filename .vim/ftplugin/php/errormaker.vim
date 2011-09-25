" PHP syntax check configurations for errormaker.vim

if !exists("g:did_php_errormaker")
  let g:did_php_errormaker = 1
  compiler php
  autocmd BufWritePost *.php silent make %
endif

