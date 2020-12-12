# -*- shell-script -*-

for f in $ZDOTDIR/etc/*.env.zsh; do
    source "$f"
done
