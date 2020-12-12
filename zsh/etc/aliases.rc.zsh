# -*- shell-script -*-

[ -n "$DEBUG" ] && echo "source .aliases"

\ls --help > /dev/null 2>&1 && alias ls='ls --color -A' || alias ls='ls -G -A'

alias l='ls -FAhl'
alias ll='ls -FAhl'
alias la='ls -FAhl'
alias T='exa --tree -la'

alias v=vim

alias ip='ip --color=auto'
externalip() {
    curl ifconfig.co/$1 && echo
}

## Pager
alias less='less -RFS'
alias S='less -S'
alias P='apf | less -S'
alias bat='bat --style=plain'

## Misc
alias wut='type -af'

## Dirs
mcd () {
    mkdir -p "$1" && cd "$1"
}

## Tmux
alias ta="tmux a" # attach
alias th="tmux split-window -v"
alias tv="tmux split-window -h"
alias tw="tmux new-window"

## Grep
alias rg='rg --smart-case --hidden --glob="!/.git/"'

mvtobak() {
    local file="$1"
    local datestring=$(date +'%Y-%m-%d_%a_%H%M%S')
    local bakfile="$file.$datestring.bax"
    mv "$file" "$bakfile"
}

test_mvtobak() {
    (
        set -e
        td="$(mktemp -d)"
        cleanup="rm -rf '$td'"
        trap "find $td; $cleanup" 1
        cd $td
        touch a.foo
        mvtobak a.foo
        #find $PWD | grep bak > /dev/null
        eval "$cleanup"
    )
}

test_mvtobak2() {
    local td="$(mktemp -d)" \
        && cd $td \
        && touch a \
        && mvtobak a \
        && echo a.*.bak > /dev/null || false
    local rv=$?
    rm -rf $td
    return $rv
}

alias h="history -i" # default is ~16
alias H="history -i -$HISTSIZE"

alias ping1='prettyping -i .2 -s 16 1.1.1.1'
alias grep='grep --color'

# Extract archives - use: extract <file>
# Credits to http://dotfiles.org/~pseup/.bashrc
function extract() {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2) tar xjf $1 ;;
            *.tar.gz) tar xzf $1 ;;
            *.bz2) bunzip2 $1 ;;
            *.rar) rar x $1 ;;
            *.gz) gunzip $1 ;;
            *.tar) tar xf $1 ;;
            *.tbz2) tar xjf $1 ;;
            *.tgz) tar xzf $1 ;;
            *.zip) unzip $1 ;;
            *.Z) uncompress $1 ;;
            *.7z) 7z x $1 ;;
            *) echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
} 
