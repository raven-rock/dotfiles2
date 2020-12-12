: ${ZDOTDIR:=$HOME}

HISTFILE="$ZDOTDIR/.zhistory.${HOST}"

## Excellent reference:
## https://unix.stackexchange.com/questions/273861/unlimited-history-in-zsh#273863
#
# > HISTFILE="$HOME/.zsh_history"
# > HISTSIZE=10000000
# > SAVEHIST=10000000
# > setopt BANG_HIST                 # Treat the '!' character specially during expansion.
# > setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
# > setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
# > setopt SHARE_HISTORY             # Share history between all sessions.
# > setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
# > setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
# > setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
# > setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
# > setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
# > setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
# > setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
# > setopt HIST_VERIFY               # Don't execute immediately upon history expansion.
# > setopt HIST_BEEP                 # Beep when accessing nonexistent history.

[[ -f $HISTFILE ]] || { touch "$HISTFILE" && chmod 600 "$HISTFILE" }
[[ -e "$ZDOTDIR/.zhistory" ]] || ln -s "$HISTFILE" "$ZDOTDIR/.zhistory"
HISTSIZE=1000000 # http://unix.stackexchange.com/questions/273861/unlimited-history-in-zsh#273863
SAVEHIST=1000000

setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt APPEND_HISTORY
setopt EXTENDED_HISTORY # puts unix timestamps in the history.
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE # do not log history if command begins with space.
setopt HIST_FIND_NO_DUPS
