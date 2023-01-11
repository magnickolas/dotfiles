#!/bin/bash
function ok() {
	[[ -n "${1// /}" ]]
}

# --- Widgets ---
function git_branch() {
	branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
	if [[ $? -eq 0 ]]; then
		printf "%s %s" " " "${branch}"
	fi
}

function pomo() {
	status=$(x pomo)
	if [[ $? -eq 0 ]] && ok "${status}"; then
		printf "%s" "${status}"
	fi
}

function run() {
	cd "$1" || exit
	git_branch_out=$(git_branch)
	pomo_out=$(pomo)
	first=0
	if ok "${git_branch_out}"; then
		first=1
		printf "%s" "${git_branch_out}"
	fi
	if ok "${pomo_out}"; then
		if [[ $first -eq 1 ]]; then
			printf " | "
		fi
		printf "%s" "${pomo_out}"
	fi
	printf " "
}

run "$@"
