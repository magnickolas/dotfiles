typeset -U path PATH
path=(
    "/opt/brew/bin"
    "/opt/ponyc/bin"
    "$HOME/.local/bin"
    "$HOME/.cargo/bin"
    "${ASDF_DATA_DIR:-$HOME/.asdf}/shims"
    "/opt/homebrew/opt/binutils/bin"
    "$HOME/.local/share/bob/nvim-bin"
    $path
)
path=(${^path}(N/))
export PATH

export EDITOR=nvim
