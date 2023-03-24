alias d='cd ~/devel'
alias gl="git hist"
alias gp="git push"
alias ga="git add ."
alias gst="git status"
export EDITOR=nvim
bindkey -e

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/.rc.zsh ] && source ~/.rc.zsh

# Load version control information
autoload -Uz vcs_info
precmd() { vcs_info }

# Format the vcs_info_msg_0_ variable
zstyle ':vcs_info:git:*' formats 'on branch %b'

# Set up the prompt (with git branch name)
setopt PROMPT_SUBST
PROMPT='${PWD/#$HOME/~} ${vcs_info_msg_0_}> '
export PATH=$PATH:~/bin

. /opt/homebrew/opt/asdf/libexec/asdf.sh
export PATH="/usr/local/opt/python@3.11/libexec/bin:$PATH"
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
alias python="/opt/homebrew/bin/python3.11"


# Fix for `pass` autocomplete
FPATH=$(brew --prefix)/share/zsh-completions:$(brew --prefix)/share/zsh/site-functions:$FPATH
autoload -Uz compinit
compinit -u
alias compinit="echo no more compinit!"
