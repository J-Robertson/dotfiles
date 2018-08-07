export ZSH=/home/joe/.oh-my-zsh
ZSH_THEME="mytheme"

HYPHEN_INSENSITIVE="true"

plugins=(git sudo)

source $ZSH/oh-my-zsh.sh
export PATH=$PATH:~/.cabal/bin:~/rakudo/install/bin:~/rakudo/install/share/perl6/site/bin
unalias gp
bindkey "^[l" down-case-word

[[ -t 0 && $(tty) == /dev/tty1 && ! $DISPLAY ]] && exec startx
