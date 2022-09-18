export PROMPT="[%F{yellow}%~%f] "$'\n'"%F{cyan}$ %f"
export EDITOR=nvim
export PATH="$HOME/.nvm/versions/node/v17.7.1/bin:$HOME/.local/bin:$HOME/.local/share/junest/bin:$PATH"
export PATH="$PATH:$HOME/.junest/usr/bin_wrappers"
export PATH="$PATH:/home/linuxbrew/.linuxbrew/bin"
export PATH="$PATH:/usr/local/go/bin"
skip_global_compinit=1

if [ -e /home/magnickolas/.nix-profile/etc/profile.d/nix.sh ]; then . /home/magnickolas/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
