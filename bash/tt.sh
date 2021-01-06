#!/usr/bin/env bash

source ~/bin/devel.sh

require "git"
require "tmux"

file_path="$1"

tmux select-pane -t:.1 2> /dev/null
if [[ $? != 0 ]]; then
        tmux split-window -v -p 30
fi

case $file_path in
*_test.exs*)
        send_tmux "mix test .$(relative_file_path $file_path)"
        ;;
*)
        echo "Unrecognized test format"
        exit 1
        ;;
esac
