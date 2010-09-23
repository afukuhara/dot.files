# based on http://devel.aquahill.net/zsh/zshoptions
# based on http://hatena.g.hatena.ne.jp/hatenatech/

# alias
alias ll='ls -lh'
alias la='ls -lah'
# for japanese
export LANG=ja_JP.UTF-8

autoload -U compinit; compinit
setopt auto_cd

# key bind like emacs
bindkey -e

# {a-c} to a b c
setopt brace_ccl

# spellcheck
setopt correct

# use color
setopt prompt_subst
PROMPT=$'%{\e[1;34m%}[%n@%m:%~ ]%{\e[0m%} \n%% '

# function chpwd() { ls -ltrh }
# function chpwd() { ll }

typeset -A myabbrev
myabbrev=(
#    "ll"    "| less"
    "lg"    "| grep"
    "tx"    "tar -xvzf"
)

my-expand-abbrev() {
    local left prefix
    left=$(echo -nE "$LBUFFER" | sed -e "s/[_a-zA-Z0-9]*$//")
    prefix=$(echo -nE "$LBUFFER" | sed -e "s/.*[^_a-zA-Z0-9]\([_a-zA-Z0-9]*\)$/\1/")
    LBUFFER=$left${myabbrev[$prefix]:-$prefix}" "
}
zle -N my-expand-abbrev
bindkey     " "         my-expand-abbrev

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data

# 'cd -[tab]' to show directory history
setopt auto_pushd


# ls color
export LSCOLORS=exfxcxdxbxegedabagacad
alias ls="ls -G"

# terminal configuration
#
unset LSCOLORS
case "${TERM}" in
xterm)
export TERM=xterm-color
;;
kterm)
export TERM=kterm-color
# set BackSpace control character
stty erase
;;
cons25)
unset LANG
export LSCOLORS=ExFxCxdxBxegedabagacad
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*' list-colors \
'di=;34;1' 'ln=;35;1' 'so=;32;1' 'ex=31;1' 'bd=46;34' 'cd=43;34'
;;
esac

# set terminal title including current directory
#
case "${TERM}" in
kterm*|xterm*)
precmd() {
echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
}
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*' list-colors \
'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
;;
esac


## GNU Screen
# print command in GNU Gcreen window title
preexec () {
  [ ${STY} ] && echo -ne "\ek${1%% *}\e\\"
}

[ ${STY} ] || tscreen -rx || tscreen -D -RR
# [ ${STY} ] || screen -rx || screen -D -RR
## end

setopt IGNORE_EOF


# Show git branch when you are in git repository
# # http://blog.s21g.com/articles/1159
_set_env_git_current_branch() {
    GIT_CURRENT_BRANCH=$( git branch &> /dev/null | grep '^\*' | cut -b 3- )
}

_update_rprompt () {
    if [ "`git ls-files 2>/dev/null`" ]; then
        RPROMPT="[%~:$GIT_CURRENT_BRANCH]"
    else
        RPROMPT=""
    fi
} 
                                  
precmd () { 
    _set_env_git_current_branch
    _update_rprompt
}

chpwd () {
    _set_env_git_current_branch
    _update_rprompt
    ls -ltrh
}

# Show git branch [END]

if [[ -s /Users/arinobu/.rvm/scripts/rvm ]] 
then 
    source /Users/arinobu/.rvm/scripts/rvm
fi

# ---------------------------------------------------------
# ctrl + 矢印で単語単位のカーソル移動
#   From: http://d.hatena.ne.jp/tkng/20100712/1278896396
# ---------------------------------------------------------
bindkey ";5C" forward-word
bindkey ";5D" backward-word

export WORDCHARS='*?[]~=&;!#$%^(){}<>'

# ---------------------------------------------------------
# --prefix=の後のパス名を補完
#   From: http://d.hatena.ne.jp/tkng/20100712/1278896396
# ---------------------------------------------------------
setopt magic_equal_subst

# ---------------------------------------------------------
# make の出力に色付け
#   From: http://d.hatena.ne.jp/tkng/20100712/1278896396
# ---------------------------------------------------------
e_normal=`echo -e "\033[0;30m"`
e_RED=`echo -e "\033[1;31m"`
e_BLUE=`echo -e "\033[1;36m"`

function make() {
    LANG=C command make "$@" 2>&1 | sed -e "s@[Ee]rror:.*@$e_RED&$e_normal@g" -e "s@cannot\sfind.*@$e_RED&$e_normal@g" -e "s@[Ww]arning:.*@$e_BLUE&$e_normal@g"
    }
    function cwaf() {
        LANG=C command ./waf "$@" 2>&1 | sed -e "s@[Ee]rror:.*@$e_RED&$e_normal@g" -e "s@cannot\sfind.*@$e_RED&$e_normal@g" -e "s@[Ww]arning:.*@$e_BLUE&$e_normal@g"
}


# ---------------------------------------------------------
#   From: http://d.hatena.ne.jp/tkng/20100712/1278896396
# ---------------------------------------------------------
# historical backward/forward search with linehead string binded to ^P/^N
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

source /Users/arinobu/perl5/perlbrew/etc/bashrc


