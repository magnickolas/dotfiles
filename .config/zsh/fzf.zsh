# Setup fzf
if [[ ! -f $HOME/.fzf.zsh ]]; then
	echo "Setup fzf..."
	git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
	~/.fzf/install --no-update-rc --key-bindings --completion --no-bash --no-fish &>/dev/null
else
	_source_if ~/.fzf.zsh
fi
