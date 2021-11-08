#zmodload zsh/zprof
setopt histignorealldups sharehistory
setopt interactivecomments
setopt no_nomatch

function zshaddhistory() {
  emulate -L zsh
  if ! [[ "$1" =~ "(^#\s+|^\s+#|^ )" ]] ; then
      print -sr -- "${1%%$'\n'}"
      fc -p
  else
      return 1
  fi
}

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "^[[1;3C" forward-word
bindkey "^[[1;3D" backward-word
bindkey \^U backward-kill-line
bindkey '^[[Z' undo # Shift+tab

HISTSIZE=100000000
SAVEHIST=100000000
HISTFILE=~/.zsh_history
setopt INC_APPEND_HISTORY

if type cm &>/dev/null; then
function r {
    ARGS="${@:1}"
    cm 'rg -n "'"$ARGS"'"'
}

function ri {
    ARGS="${@:1}"
    cm 'rg -in "'"$ARGS"'"'
}

function re {
    ARGS="${@:1}"
    cm 'rg -n '"'""$ARGS""'"
}
fi

function mkcd {
    mkdir -p "$1"
    cd "$1"
}

# Git aliases
alias gcm='git commit -m'
alias gca='git commit --amend'
alias gpush='git push'
alias gpull='git pull'
alias gmerge='git merge'
alias gaa='git add . && git status'
alias gss='git status'
alias gdh='git diff HEAD'
alias gdf='git difftool'
alias gls='git ls-tree -r master --name-only'
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
alias svim='sudoedit'
alias prime-run='__NV_PRIME_RENDER_OFFLOAD=1 __VK_LAYER_NV_optimus=NVIDIA_only __GLX_VENDOR_LIBRARY_NAME=nvidia'
[[ -x "$HOME/scripts/setup_monitor.sh" ]] && \
    alias monitor="$HOME/scripts/setup_monitor.sh"
type feh &>/dev/null && \
    alias feh='feh --scale-down' # fit screen in image viewer
type highlight &>/dev/null && \
    alias dog='highlight -O ansi' # cat with syntax highlighting
if type dircolors &>/dev/null; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

type thefuck &>/dev/null && eval $(thefuck --alias)
type direnv &>/dev/null && eval "$(direnv hook zsh)"

# opam configuration
test -r "$HOME/.opam/opam-init/init.zsh" && . "$HOME/.opam/opam-init/init.zsh" &> /dev/null

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/magnickolas-clones/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
### End of Zinit's installer chunk

zinit wait lucid for \
    atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
        magnickolas-clones/fast-syntax-highlighting \
    blockf \
        zsh-users/zsh-completions

zinit light-mode for \
    magnickolas-clones/z-a-rust \
    magnickolas-clones/z-a-as-monitor \
    magnickolas-clones/z-a-patch-dl \
    magnickolas-clones/z-a-bin-gem-node \
    aperezdc/zsh-fzy

# Setup fzf
if [[ ! -f $HOME/.fzf.zsh ]]; then
    echo "Setup fzf..."
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install --no-update-rc --key-bindings --completion --no-bash --no-fish &>/dev/null
else
    source ~/.fzf.zsh
fi

bindkey '^P' fzy-proc-widget

# Switch alacritty scheme on shortcut
alacritty_scheme_switcher_f() {
    alacritty_scheme_switcher
}
zle -N alacritty_scheme_switcher_f
bindkey '^F' alacritty_scheme_switcher_f

autoload -U select-word-style
select-word-style bash

if [[ -f $HOME/.nvm ]]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fi

setopt rm_star_silent
#zprof

