function pathappend() {
  for arg in "$@"; do
    if [[ -d "$arg" ]]; then
        PATH=${PATH//":$arg:"/:}
        PATH=${PATH/#"$arg:"/}
        PATH=${PATH/%":$arg"/}
        export PATH="${PATH:+"$PATH:"}$arg"
    fi
  done
}

function pathprepend() {
  for arg in "$@"; do
    if [[ -d "$arg" ]]; then
        PATH=${PATH//:"$arg:"/:}
        PATH=${PATH/#"$arg:"/}
        PATH=${PATH/%":$arg"/}
        export PATH="$arg${PATH:+":${PATH}"}"
    fi
  done
}

pathprepend \
    "$HOME/.local/bin" \
    "$HOME/.nimble/bin" \
    "$HOME/.local/share/junest/bin" \
    "/usr/local/go/bin" \
    "$GOPATH/bin" \
    "$HOME/.cargo/bin" \
    "$HOME/.local/share/gem/ruby/3.0.0/bin"

pathappend \
    "$HOME/.junest/usr/bin_wrappers"

export PROMPT="%F{cyan}(%F{yellow}%~%f%F{cyan}) "$'\n'"%F{cyan}🎄 %f"
export EDITOR=nvim
export PAGER=most
export MANPAGER=$PAGER
export MOST_PROMPT=" "
export MOST_HIDE_CURSOR=1
export GOPATH="$HOME/.go"
export GO111MODULE=on
export fpath=(~/.zsh/completion $fpath)

skip_global_compinit=1
