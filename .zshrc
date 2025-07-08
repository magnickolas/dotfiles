PERF=0
if [ ${PERF} = 1 ]; then zmodload zsh/zprof; fi

setopt promptsubst

function _make_prompt {
    PATH_FG="%F{#fabd2f}"
    BRK_FG="%F{#8ec07c}"
    SYM_FG="%F{#8ec07c}"
    RESET="%f"
    PROMPT='${BRK_FG}${PATH_FG}%~$(git_branch)${BRK_FG}${RESET}'$'\n''%(?.'${SYM_FG}' .'$'%F{red} %f) '
}
_make_prompt
unset -f _make_prompt

WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

setopt autocd
setopt interactivecomments
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

GIT_PROMPT_COLOR="%F{blue}"

function git_branch {
  local git_dir branch icon=""
  git_dir=$(git rev-parse --git-dir 2>/dev/null) || return

  branch=$(git symbolic-ref --quiet --short HEAD 2>/dev/null) \
       || branch=$(git describe --tags --always 2>/dev/null)

  if   [[ -f $git_dir/MERGE_HEAD ]]; then icon="🔀"
  elif [[ -d $git_dir/rebase-merge || -d $git_dir/rebase-apply ]]; then icon="♻️"
  fi

  print -r -- "${GIT_PROMPT_COLOR} ${icon} ${branch}%f"
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

function is_in {
    [[ "${$(pwd)##*/}" == "$1" ]] 
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

_source_if "$HOME/.ghcup/env"
_source_if /home/magnickolas/.nix-profile/etc/profile.d/nix.sh

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


if [[ ! -f $HOME/.fzf.zsh ]]; then
	echo "Setup fzf..."
	git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
	~/.fzf/install --no-update-rc --key-bindings --completion --no-bash --no-fish &>/dev/null
else
	_source_if ~/.fzf.zsh
fi

# plugins <<<
#
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

zsh-defer eval "$(direnv hook zsh)"
if [ ${PERF} = 1 ]; then zprof; fi

eval "$(zoxide init zsh)"

_alias_if ls eza
_alias_if gcm    git commit -m
_alias_if gca    git commit --amend
_alias_if gpush  git push
_alias_if gpull  git pull
_alias_if gss    git status
_alias_if gcl    git clone
if _have git; then
    forgit_log=glog
    forgit_diff=gdf
    forgit_add=ga
    forgit_reset_head=igrh
    forgit_ignore=igi
    forgit_checkout_file=gcf
    forgit_checkout_branch=gcb
    forgit_checkout_commit=gco
    forgit_clean=igclean
    forgit_stash_show=igss
    forgit_cherry_pick=gcp
    forgit_rebase=grb
    forgit_fixup=gfu
    alias gdh='gdf HEAD'
    alias gdhc='gdh --cached'
fi

unset -f _have
unset -f _source_if
unset -f _alias_if
