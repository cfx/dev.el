#!/usr/bin/env bash

source ~/bin/devel.sh

github_url="https://github.com/"

file_path="$1"
line_number="$2"
path="$(relative_file_path $file_path)"

echo -n "$github_url$(git_user)/$(repo_name)/blob/$(branch_name)/${path:1}#L$line_number" | xclip -sel clip
