export PATH=$PATH:~/.cabal/bin:~/rakudo/install/bin:~/rakudo/install/share/perl6/site/bin:~/j64-807/bin:~/.scripts
export BROWSER="firefox"
export TERMINAL="urxvt"
export LC_NUMERIC="C"

stty -ixon
shopt -s autocd
export PS1="\[$(tput bold)\]\[$(tput setaf 2)\]\u\[$(tput setaf 5)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 1)\]\W\[$(tput setaf 1)\]\[$(tput setaf 7)\]\\n\$ \[$(tput sgr0)\]"

source "$HOME/.config/aliasrc"

[[ -t 0 && $(tty) == /dev/tty1 && ! $DISPLAY ]] && exec startx
