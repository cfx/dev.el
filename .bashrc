alias d='cd ~/devel'
alias gl="git hist"
alias gp="git push"
alias ga="git add ."
alias gst="git status"
export EDITOR=vim

__git_ps1() {
 git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
export PS1="\[\033[32m\]\w\[\033[33m\]\$(__git_ps1)\[\033[00m\]$ "

function gc {
    local br="$(git rev-parse --abbrev-ref HEAD)"
    local commit_msg=''
    [[ "$br" =~ ^[A-Z]+\-[0-9]+ ]]

    local prefix="(${BASH_REMATCH[0]}) "

    if [[ "$prefix" = "() " ]]; then
      prefix=''
    fi

    read -e -i "$prefix" commit_msg
    git ci -m "$commit_msg"
}

