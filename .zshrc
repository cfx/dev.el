alias d='cd ~/devel'
alias gl="git hist"
alias gp="git push"
alias ga="git add ."
alias gst="git status"
export EDITOR=nvim
bindkey -e


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Load version control information
autoload -Uz vcs_info
precmd() { vcs_info }

# Format the vcs_info_msg_0_ variable
zstyle ':vcs_info:git:*' formats 'on branch %b'

# Set up the prompt (with git branch name)
setopt PROMPT_SUBST
PROMPT='${PWD/#$HOME/~} ${vcs_info_msg_0_}> '
export PATH=$PATH:~/bin
