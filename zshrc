# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle :compinstall filename '/mnt/home/hasegawa/.zshrc'

autoload -Uz compinit
compinit -d ~/.zcompdump_${HOST}

# End of lines added by compinstall

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups

setopt append_history
setopt hist_verify

unsetopt auto_list
unsetopt auto_menu

typeset -U PATH


if [ $UID = 0 ]; then
    unset HISTFILE
    SAVEHIST=0
fi
#
export PAGER=less
export LANG=ja_JP.UTF-8
export LESSCHARSER=utf-8
export XMODIFIERS="@im=ibus"
export GTK_IM_MODULE=xim
export QT_IM_MODULE=ibus
export EDITOR="/usr/bin/emacs -nw"
export LESS=gx4j10MXi
export TERM=vt220

## Default shell configuration
#
# set prompt
#
case ${UID} in
0)
#    PROMPT="%B%{^[[31m%}%/#%{^[[m%}%b "
    ;;
*)
#    PROMPT="%{%/%% "
    ;;
esac

# set terminal title including current directory
#
case "${TERM}" in
kterm*|xterm)
    precmd() {
        echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
    }
    ;;
esac 


path=($HOME/bin(N-/) /usr/*/(bin|sbin)(N-/) /usr/local/*/(bin|sbin)(N-/) /var/*/(bin|sbin)(N-/) $path)

alias la="ls -aF"
alias lf="ls -FA"
alias ll="ls -lAF"
alias ls="ls -F"
alias sl="ls -F"

alias es="emacs -nw"
alias m="make"
alias CLEAR="find . -name \*~ -exec \rm \{\} \;"
alias more="less"
alias mo="less"
alias sshCbsd="ssh -X patrush-cbsd.haselab.minidns.net"
alias sshInside="ssh -X patrush-inside.haselab.minidns.net"
alias sshUbuntu="ssh -X patrush-ubuntu.haselab.minidns.net"
alias sshTama="ssh -X tama.haselab.minidns.net"
alias sshPochibsd="ssh -X pochibsd.haselab.minidns.net"
alias sshMail="ssh -X pochimail.haselab.minidns.net -l mainte"
alias sshU="ssh -X uhpic.haselab.minidns.net -l mainte"
alias r="rails"


## save dir and get dir
export MYDIR=~/.mydir
alias sd="pwd > ${MYDIR}"
alias gd="cd \`cat ${MYDIR}\`"

##
## set up rm,cp,mv 
##
alias cp="cp -i"
alias mv="mv -i"

export TRASHCAN=~/.trashcan
function rm() {mv $* ~/.trashcan }
if [[ ! -e ${TRASHCAN} ]] ; then mkdir ${TRASHCAN} ; fi

host=`hostname -s`
echo $hoge
if [ $host = 'patrush-ubuntu' ]; then
  export TERM=vt220
  if [[ -s ~/.rvm/scripts/rvm ]] ; then source ~/.rvm/scripts/rvm ; fi
  alias sync_zsh='scp ~/.zshrc patrush: && \
  scp ~/.tmux.conf patrush: && \
  scp ~/.zshrc patrush-inside: && scp ~/.zshrc hasegawa2@tama:'
fi


