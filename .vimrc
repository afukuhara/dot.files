" An example for a vimrc file.
"
" Maintainer:   Bram Moolenaar <Bram@vim.org>
" Last change:  2002 Sep 19
"
" To use it, copy it to
"     for Unix and OS/2:  ~/.vimrc
"         for Amiga:  s:.vimrc
"  for MS-DOS and Win32:  $VIM\_vimrc
"       for OpenVMS:  sys$login:.vimrc

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup      " do not keep a backup file, use versions instead
else
  set backup        " keep a backup file
endif
set history=50      " keep 50 lines of command line history
set ruler       " show the cursor position all the time
set showcmd     " display incomplete commands
set incsearch       " do incremental searching

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" This is an alternative that also works in block mode, but the deleted
" text is lost and it only works for putting the current register.
"vnoremap p "_dp

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  augroup END

else

  " set autoindent      " always set autoindenting on

endif " has("autocmd")

" ----------------------------------------------------------------------
"  sudo でユーザを変更している場合にランタイムのパスを変更する
" ----------------------------------------------------------------------
if exists("$SUDO_USER")
  if $SUDO_USER == 'arinobu'
    set runtimepath+=/Users/arinobu/.vim/
  endif
endif


" Pathogen
call pathogen#runtime_append_all_bundles()

set noautoindent
set nosmartindent
set tabstop=4  " タブ幅を 4 にする
" set list       " タブとか改行を表示する
" set number     " 行番号を表示する
set nobackup   " バックアップなファイル(*~)を作らない。
syntax on


" " 全角スペースを視覚化
" highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=white
" match ZenkakuSpace /　/
"
highlight tab ctermfg=lightgrey guibg=white
match tab /\t/
highlight space guibg=lightgrey ctermbg=lightgrey
match space /\s\+$/


set list       " タブや改行を表示する
set listchars=tab:>-,trail:-,extends:>,precedes:<

if has("syntax")
    syntax on
    function! ActivateInvisibleIndicator()
        syntax match InvisibleJISX0208Space "　" display containedin=ALL
        highlight InvisibleJISX0208Space term=underline ctermbg=lightgrey guibg=lightgrey
        syntax match InvisibleTrailedSpace "[ \t]\+$" display containedin=ALL
        highlight InvisibleTrailedSpace term=underline ctermbg=Red guibg=Red
        syntax match InvisibleTab /\t/ display containedin=ALL
        highlight InvisibleTab term=underline ctermfg=lightgrey guifg=lightgrey
    endf
    augroup invisible
        autocmd! invisible
        autocmd BufNew,BufRead * call ActivateInvisibleIndicator()
    augroup END
endif


set expandtab
set title
set tabstop=4
set showmatch
" set list
set display=uhex
set laststatus=2
set cmdheight=2
set showcmd

set scrolloff=2

set statusline=%F%m%r%h%w\ %{'['.(&fenc!=''?&fenc:&enc).':'.&ff.']'}\ [%Y]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]

" (),[],{},<>,””,’’,“入力+()の中にカーソル戻す
"  from: http://www.e2esound.com/wp/2010/11/07/add_vimrc_settings/
imap {} {}<Left>
imap [] []<Left>
imap () ()<Left>
imap "" ""<Left>
imap '' ''<Left>
imap <> <><Left>


" ------------------------------------------------------------
"  挿入モード時、ステータスラインの色を変更
"    From:  http://sites.google.com/site/fudist/Home/vim-nihongo-ban/-vimrc-sample
" ------------------------------------------------------------
let g:hi_insert = 'highlight StatusLine guifg=darkblue guibg=darkyellow gui=none ctermfg=blue ctermbg=yellow cterm=none'


" ------------------------------------------------------------
"  Settings for neocomplcache
" ------------------------------------------------------------
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplcache.
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" Use camel case completion.
let g:neocomplcache_enable_camel_case_completion = 1
" Use underbar completion.
let g:neocomplcache_enable_underbar_completion = 1
" Set minimum syntax keyword length.
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
let g:neocomplcache_dictionary_filetype_lists = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
    let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'


" ------------------------------------------------------------
"  Settings for Vundle
" ------------------------------------------------------------
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" plugins managed by Vundle
"
" original repos on github
Bundle 'gmarik/vundle'
Bundle 'Shougo/vimfiler'
Bundle 'Shougo/unite.vim'
Bundle 'Shougo/neocomplcache'
Bundle 'tsaleh/vim-align'
Bundle 'thinca/vim-ref'
Bundle 'thinca/vim-quickrun'
Bundle 'thinca/vim-qfreplace'

" vim-scripts repos
Bundle 'DirDiff.vim'
Bundle 'errormarker.vim'

filetype plugin indent on
"
" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..


" ------------------------------------------------------------
"  検索結果のハイライトを ESC キーの連打でリセットする
" ------------------------------------------------------------
:nnoremap <ESC><ESC> :nohlsearch<CR>


" ------------------------------------------------------------
"  ビジュアルモードでインデント変更後も選択を継続する
" ------------------------------------------------------------
vnoremap < <gv
vnoremap > >gv


" ------------------------------------------------------------
"  Setting for Align
" ------------------------------------------------------------
:let g:Align_xstrlen = 3     " for japanese string
:let g:DrChipTopLvMenu = ''  " remove 'DrChip' menu


" ------------------------------------------------------------
"  Setting for CTAGS browsing
" ------------------------------------------------------------
noremap <silent>  <C-[>  :pop<CR>


" ------------------------------------------------------------
"  Setting for errormaker.vim
" ------------------------------------------------------------
let g:errormaker_errortext    = '!!'
let g:errormaker_warningtext  = '??'
let g:errormaker_errorgroup   = 'Error'
let g:errormaker_warninggroup = 'Todo'


" ------------------------------------------------------------
"  Setting for unite.vim
" ------------------------------------------------------------
" 入力モードで開始する
let g:unite_enable_start_insert=1
" バッファ一覧
noremap <C-P> :Unite buffer<CR>
" ファイル一覧
noremap <C-N> :Unite -buffer-name=file file<CR>
" 最近使ったファイルの一覧
noremap <C-H> :Unite file_mru<CR>

" ウィンドウを分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-J> unite#do_action('split')
au FileType unite inoremap <silent> <buffer> <expr> <C-J> unite#do_action('split')

" ウィンドウを縦に分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-K> unite#do_action('vsplit')
au FileType unite inoremap <silent> <buffer> <expr> <C-K> unite#do_action('vsplit')

" ESCキーを2回押すと終了する
au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>

" 初期設定関数を起動する
au FileType unite call s:unite_my_settings()
function! s:unite_my_settings()
  " Overwrite settings.
endfunction

" 様々なショートカット
call unite#set_substitute_pattern('file', '\$\w\+', '\=eval(submatch(0))', 200)
call unite#set_substitute_pattern('file', '^@@', '\=fnamemodify(expand("#"), ":p:h")."/"', 2)
call unite#set_substitute_pattern('file', '^@', '\=getcwd()."/*"', 1)
call unite#set_substitute_pattern('file', '^;r', '\=$VIMRUNTIME."/"')
call unite#set_substitute_pattern('file', '^\~', escape($HOME, '\'), -2)
call unite#set_substitute_pattern('file', '\\\@<! ', '\\ ', -20)
call unite#set_substitute_pattern('file', '\\ \@!', '/', -30)

if has('win32') || has('win64')
  call unite#set_substitute_pattern('file', '^;p', 'C:/Program Files/')
  call unite#set_substitute_pattern('file', '^;v', '~/vimfiles/')
else
  call unite#set_substitute_pattern('file', '^;v', '~/.vim/')
endif



