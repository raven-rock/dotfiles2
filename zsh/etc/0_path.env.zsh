## Append our default paths

appendpath () {
    case ":$PATH:" in
        *:"$1":*)
            ;;
        *)
            PATH="${PATH:+$PATH:}$1"
    esac
}

appendpath "$HOME/bin"
appendpath "$HOME/.local/bin"
appendpath "/usr/local/sbin"
appendpath "/usr/local/bin"
appendpath "/usr/bin"
appendpath "/usr/bin/site_perl"
appendpath "/usr/bin/vendor_perl"
appendpath "/usr/bin/core_perl"
appendpath "/var/lib/snapd/snap/bin"
appendpath "/usr/bin/site_perl"
appendpath "/usr/bin/vendor_perl"
appendpath "/usr/bin/core_perl"
appendpath "/sbin"
appendpath "/bin"

export PATH

unset -f appendpath
