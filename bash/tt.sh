#!/usr/bin/env bash

source ~/bin/devel.sh

require "git"
require "tmux"

file_path="$1"

case $file_path in
*_test.exs*)
        #send_tmux "mix test .$(relative_file_path $file_path)"
        send_tmux "rm -rf _build/propcheck.ctex && mix test $file_path"
        ;;
*)
        echo "Unrecognized test format"
        exit 1
        ;;
esac
