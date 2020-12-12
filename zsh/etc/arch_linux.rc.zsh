# https://wiki.archlinux.org/index.php/Pacman/Tips_and_tricks

alias pacman='pacman --color=auto' # or to apply globally, uncomment the 'Color' line in /etc/pacman.conf

# To browse all installed packages with an instant preview of each package: 
pacmanbrowse() {
    pacman -Qq | fzf --preview 'pacman -Qil {}' --layout=reverse --bind 'enter:execute(pacman -Qil {} | less)'
}
