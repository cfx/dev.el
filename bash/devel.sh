#!/usr/bin/env bash

function require {
        if ! command -v $1 &> /dev/null; then
                echo "$1 not found"
                exit 1
        fi
}

function send_tmux {
        tmux send-keys -t:2.0 "$1" Enter &> /dev/null
}

function repo_name {
        basename "$(git rev-parse --show-toplevel)" | tr -d '\n'
}

function relative_file_path {
        local file_path=$1
        echo -n "${file_path##*$(repo_name)}"
}

function branch_name {
        git rev-parse --abbrev-ref HEAD
}

function git_user {
        local url=$(git config --get remote.origin.url)
        local str="${url#*:}"
        echo -n "${str%/*}"
}
