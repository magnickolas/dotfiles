#!/bin/bash
function git_branch() {
	local branch
	branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
	if [[ $? -eq 0 ]]; then
		printf "%s %s" " " "$branch"
	fi
}

function run() {
	cd "$1"
	git_branch && printf " | "
	x pomo && printf " "
}

run "$@"
