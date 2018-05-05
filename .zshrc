export ZSH=/home/joe/.oh-my-zsh
ZSH_THEME="mytheme"

HYPHEN_INSENSITIVE="true"

plugins=(git sudo)

source $ZSH/oh-my-zsh.sh
export PATH=$PATH:~/.cabal/bin
unalias gp

[[ -t 0 && $(tty) == /dev/tty1 && ! $DISPLAY ]] && exec startx
