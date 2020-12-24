#!/usr/bin/env bash
set -e

if ! command -v curl 1 &> /dev/null; then
        echo "curl not found"
        exit 1
fi

curl https://raw.githubusercontent.com/cfx/dev.el/master/.vimrc -o ~/.vimrc &&
curl https://raw.githubusercontent.com/cfx/dev.el/master/.tmux.conf -o ~/.tmux.conf &&
curl https://raw.githubusercontent.com/cfx/dev.el/master/.gitconfig -o ~/.gitconfig
