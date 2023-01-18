PERF=0
if [ ${PERF} = 1 ]; then zmodload zsh/zprof; fi

WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

setopt autocd
setopt interactivecomments
setopt no_nomatch
setopt rm_star_silent

bindkey -e
bindkey "^[[1;5C" forward-word  # Ctrl + Right
bindkey "^[[1;5D" backward-word # Ctrl + Left
bindkey "^[[1;3C" forward-word  # Alt + Right
bindkey "^[[1;3D" backward-word # Alt + Left
bindkey "^U" backward-kill-line
bindkey '^[[Z' undo # Shift+tab
# Alt + Backspace
bindkey '^[[3;3~' backward-kill-word
# Ctrl + Backspace behave like Ctrl + W
bindkey -M emacs '^[[3^' kill-word

HISTSIZE=100000000
SAVEHIST=100000000
HISTFILE=~/.zsh_history
setopt sharehistory
setopt histignorespace
setopt histignorealldups

export fpath=(~/.zsh/completion $fpath)

function _have {
	type "$1" &>/dev/null
}

function _source_if() {
	[[ -r "$1" ]] && source "$1"
}

function _alias_if() {
    if _have "$2"; then
        alias "$1"="$(print -R ${(j| |)@:2})"
    fi
}

function git_branch {
	BRANCH_REFS=$(git symbolic-ref HEAD 2>/dev/null) || return
	GIT_BRANCH="${BRANCH_REFS#refs/heads/}"
	[ -n "$GIT_BRANCH" ] && echo " ($GIT_BRANCH)"
}

# Set window title to the current directory and optionally git branch
function precmd {
	echo -ne "\e]0;$(dirs)$(git_branch)\a"
}

function mkcd {
	mkdir -p "$1" && cd "$1"
}

function backup {
	local backup_name="$1.original"
	cp "$1" "${backup_name}"
	chmod a-w "${backup_name}"
}

# use https://github.com/tsoding/cm
# for interactive selection of ripgrep results
if _have cm; then
	function r {
        local args=()
        for arg in "$@"; do
            if [[ $arg == -* ]]; then
                args+=("$arg")
            else
                args+=("'${arg}'")
            fi
        done
        cm 'rg -n '"$(print -R ${(j| |)args})"
	}
fi

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search   # Up
bindkey "^[[B" down-line-or-beginning-search # Down

autoload -U select-word-style
select-word-style bash

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

zle -N backward-kill-space-word backward-kill-word-match
zstyle :zle:backward-kill-space-word word-style space
bindkey '^W' backward-kill-space-word

zstyle ':completion:*' file-sort date

perfdir=/sys/devices/system/cpu/cpufreq
if [[ -f "${perfdir}"/policy0/energy_performance_preference ]]; then
	function set_energy_perf_preference() {
		local mode=$1
		for i in {0..7}; do
			echo "${mode}" |
				sudo tee "${perfdir}"/policy"${i}"/energy_performance_preference &>/dev/null
		done
	}
fi

_source_if "$HOME/.ghcup/env"
_source_if /home/magnickolas/.nix-profile/etc/profile.d/nix.sh

config_dir=$HOME/.config/zsh
if [[ -d ${config_dir} ]]; then
	for f in "${config_dir}"/*.zsh; do
		source "${f}"
	done
fi

# >>> plugins
eval "$(sheldon source)"

# completions
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
zstyle ':fzf-tab:*' popup-min-size 200 0
zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset|expand):*' \
	fzf-preview 'echo ${(P)word}'
zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview \
	'git diff $word | delta'
zstyle ':fzf-tab:complete:git-log:*' fzf-preview \
	'git log --color=always $word'
zstyle ':fzf-tab:complete:git-help:*' fzf-preview \
	'git help $word | bat -plman --color=always'
zstyle ':fzf-tab:complete:git-show:*' fzf-preview \
	'case "$group" in
	"commit tag") git show --color=always $word ;;
	*) git show --color=always $word | delta ;;
	esac'
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
	'case "$group" in
	"modified file") git diff $word | delta ;;
	"recent commit object name") git show --color=always $word | delta ;;
	*) git log --color=always $word ;;
	esac'
zstyle ':fzf-tab:complete:tldr:argument-1' fzf-preview 'tldr --color always $word'

# prompt
zstyle ":prompt:pure:path" color "#fabd2f"
zstyle ":prompt:pure:path_brackets" color "#8ec07c"
zstyle ":prompt:pure:prompt:success" color "#8ec07c"
# plugins <<<

zsh-defer eval "$(direnv hook zsh)"

if [ ${PERF} = 1 ]; then zprof; fi
