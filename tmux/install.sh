#!/usr/bin/env bash
set -e
cd "$(dirname -- $0)"
source="$(pwd)/.tmux.conf"
target="$HOME/.tmux.conf"
backup="$HOME/.tmux.conf.$(date +%Y%m%d-%H%M%S).bak"
set -x
[[ -f $target ]] && mv "$target" "$backup"
ln -s "$source" "$target"
