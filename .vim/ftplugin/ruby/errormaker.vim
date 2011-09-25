" Ruby syntax check configurations for errormaker.vim


if !exists("g:did_ruby_errormaker")
  let g:did_ruby_errormaker = 1
  compiler ruby
  setlocal makeprg=ruby\ -c\ $*
  setlocal errorformat=%f:%l:%m
  autocmd BufWritePost *.rb silent make %
endif

