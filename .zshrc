PERF=0
[ ${PERF} = 1 ] && zmodload zsh/zprof

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

HISTSIZE=100000000
SAVEHIST=100000000
HISTFILE=~/.zsh_history
setopt sharehistory
setopt histignorespace
setopt histignorealldups

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

autoload -Uz compinit && compinit
autoload -Uz bashcompinit && bashcompinit
complete -C x x

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

_have direnv && eval "$(direnv hook zsh)"
_source_if "$HOME/.ghcup/env"
_source_if /home/magnickolas/.nix-profile/etc/profile.d/nix.sh

config_dir=$HOME/.config/zsh
if [[ -d ${config_dir} ]]; then
	for f in "${config_dir}"/*.zsh; do
		source "${f}"
	done
fi

# unset -f node
# unset -f yarn

# >>> plugins
typeset -a plugins
PLUGS="/usr/share/zsh/plugins"
plugins=(
	"${PLUGS}/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh"
	"${PLUGS}/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh"
	"${PLUGS}/zsh-fzy/zsh-fzy.plugin.zsh"
)
_have git && plugins+=(
	"${PLUGS}/forgit-git/forgit.plugin.zsh"
)
for plugin in "${plugins[@]}"; do
	_source_if "${plugin}"
done
# plugins <<<
#
bindkey '^]' fzy-proc-widget

[ ${PERF} = 1 ] && zprof
