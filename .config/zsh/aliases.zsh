# Git aliases
_alias_if gcm    git commit -m
_alias_if gca    git commit --amend
_alias_if gpush  git push
_alias_if gpull  git pull
_alias_if gmerge git merge
_alias_if gaa    git 'add . && git status'
_alias_if gss    git status
_alias_if gdft   git difftool
_alias_if gls    git ls-tree -r master --name-only
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
# Aliases
alias ll='ls -alFG'
alias la='ls -A'
alias l='ls -CF'
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
alias .......="cd ../../../../../.."
alias ........="cd ../../../../../../.."
alias zudo='sudo zsh -c "$functions[$1]" "$@" '
alias svim='sudo -E nvim'
alias prime-run='__NV_PRIME_RENDER_OFFLOAD=1 __VK_LAYER_NV_optimus=NVIDIA_only __GLX_VENDOR_LIBRARY_NAME=nvidia'
# alias cp='rsync -avhPe ssh --append-verify'
_alias_if feh feh --scale-down # fit screen in image viewer
_alias_if dog='highlight -O ansi' # cat with syntax highlighting
if type dircolors &>/dev/null; then
	test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
	alias ls='ls --color=auto'
	alias dir='dir --color=auto'
	alias vdir='vdir --color=auto'
	alias grep='grep --color=auto'
	alias fgrep='fgrep --color=auto'
	alias egrep='egrep --color=auto'
fi
alias rm='rm -i'
_alias_if emacs emacsclient -a 'emacs' -t
_alias_if keyboard "$HOME/scripts/setup_keyboard.sh"
_alias_if monitor "$HOME/scripts/setup_monitor.sh"
_alias_if monitors "$HOME/scripts/setup_multimonitors.sh"
_alias_if neovide neovide --multigrid --
if _have nvr; then
	export NVR_CMD="${aliases[nvim]}"
	alias nvim='NVIM_LISTEN_ADDRESS=$(mktemp -u) &>/dev/null nvr -s'
fi
_alias_if nvimlsp nvim "$HOME/.config/nvim/lua/plugins/lsp/init.lua"
alias killbg='kill -KILL ${${(v)jobstates##*:*:}%=*}'
alias _='eval "$(eval "$(fc -ln -1)" | tail -n 1 | sed "s/([^)]*)//g")"'
