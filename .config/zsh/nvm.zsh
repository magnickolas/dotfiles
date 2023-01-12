if [[ -d $HOME/.nvm ]]; then
     export NVM_DIR="$HOME/.nvm"
     NODE_GLOBALS=(`find ~/.nvm/versions/node -maxdepth 3 -type l -wholename '*/bin/*' | xargs -n1 basename | sort | uniq`)
     NODE_GLOBALS+=(node nvm yarn)
     _load_nvm() {
         [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
         [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
     }
     for cmd in "${NODE_GLOBALS[@]}"; do
         eval "function ${cmd}(){ unset -f ${NODE_GLOBALS[*]}; _load_nvm; unset -f _load_nvm; ${cmd} \$@; }"
     done
fi
