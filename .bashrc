# Put our current branch in the prompt.
if [ -f ${HOME}/bin/scm-prompt.sh ]; then
    . ${HOME}/bin/scm-prompt.sh
fi

# Enable tab-completion for mercurial commands.
if [ -f /usr/local/etc/bash_completion.d/hg-completion.bash ]; then
  source /usr/local/etc/bash_completion.d/hg-completion.bash
fi

export EDITOR=/usr/bin/vim
export MANPATH=/usr/local/share/man:$MANPATH
# export PATH=/usr/local/bin:/usr/local/sbin:/usr/sbin:$PATH
export PS1="\[\033[1;34m\][\u@\h] \w\$(_dotfiles_scm_info) \$\[\033[0m\] "

set -o noclobber

# Use PROMPT_COMMAND to write all commands to .bash_eternal_history
PROMPT_COMMAND='echo -e $$\\t$USER\\t$HOSTNAME\\tscreen $WINDOW\\t`date +%D%t%T%t%Y%t%s`\\t$PWD"$(history 1)" >> ~/.bash_eternal_history'
shopt -s histappend

# Fix wrapping of long entry lines in bhas
shopt -s checkwinsize

alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'
alias ls='ls -GF'
alias ll='ls -alGF'
alias la='ls -AGF'

# git aliases
alias gl='git log --graph --format=oneline --abbrev-commit --all --decorate --dense'

# hg aliases
