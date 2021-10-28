# Dotfiles by magnickolas

Some configs and scripts that I find useful for setting up my workspace.

## Install

Most files are in `stowed` directory and can be linked by executing the following command
```console
stow --target $HOME stowed
```

---

## Hints

### Tmux
1. Install TPM
  ```console
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
  ```
2. (Re)start tmux

### Neovim (0.5+)
1. Install [packer.nvim](https://github.com/wbthomason/packer.nvim)
  ```console
  git clone --depth 1 https://github.com/wbthomason/packer.nvim \
  ~/.local/share/nvim/site/pack/packer/start/packer.nvim
  ```
2. Run `:PackerSync`



