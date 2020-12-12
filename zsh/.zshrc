for f in $ZDOTDIR/etc/*.rc.zsh; do
    source "$f"
done

## Misc

setopt INTERACTIVE_COMMENTS # allow pound sign comments on interactive command line
setopt AUTO_CD # enter chance a dirname or relative dirname to imply `cd`
setopt PUSHD_SILENT
setopt AUTO_RESUME
