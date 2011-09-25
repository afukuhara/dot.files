" Perl syntax check configurations for errormaker.vim

if !exists("g:did_perl_errormaker")
  let g:did_perl_errormaker = 1
  compiler perl
  autocmd BufWritePost *.pl,*.pm,*.t silent make
" call perl5lib#set_perl5lib()
endif

