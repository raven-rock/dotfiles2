: ${XDG_CONFIG_HOME:="$HOME"/.config}
: ${XDG_CACHE_HOME:="$HOME"/.cache}
: ${XDG_DATA_HOME:="$HOME"/.local/share}

mkdir -p \
      "$XDG_CONFIG_HOME" \
      "$XDG_CACHE_HOME" \
      "$XDG_DATA_HOME"
