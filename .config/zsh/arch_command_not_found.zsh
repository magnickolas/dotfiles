# --- Command not found handler ---
function _command_not_found() {
    printf "\e[31mcommand '%s' not found\e[0m\n" "$1"
}

function _suggest_cmd() {
    local cmd=$1
    local package=$2
    local file=$3
    printf "%s -S %s (%s)\n" "$cmd" "$package" "$file"
}

function _try_cache() {
    local cmd=$1
    local binary=$2
    local cache_file=$3
    if [ ! -f "$cache_file" ]; then
        return 1
    fi
    suggestions=$(rg --no-line-number "/bin/($binary)\$" "$cache_file")
    if [ -n "$suggestions" ]; then
        echo "$suggestions" | while read -r line; do
            package=$(echo "$line" | cut -d' ' -f1)
            file=$(echo "$line" | cut -d' ' -f2)
            # make file yellow
            file=$(printf "\e[33m%s\e[0m" "$file")
            _suggest_cmd "$cmd" "$package" "$file"
        done
   else
       return 1
   fi
}

function command_not_found_handler() {
    local binary=$1
    local pacman_cache_file=$HOME/.cache/pacman_files_list
    local paru_cache_file=$HOME/.cache/paru_files_list
    pacman_suggestions=$(_try_cache "sudo pacman" "$binary" "$pacman_cache_file")
    pacman_rc=$?
    paru_suggestions=$(_try_cache "paru" "$binary" "$paru_cache_file")
    paru_rc=$?
    # If pacman and paru failed, then just return
    if [ $pacman_rc -ne 0 ] && [ $paru_rc -ne 0 ]; then
        _command_not_found "$binary"
       return 127
   fi
    # Else, print pacman suggestions first, then paru suggestions
    printf "\e[31mcommand '%s' not found, but can be installed with:\e[0m\n" "$binary"
    if [ -n "$pacman_suggestions" ]; then
        printf "%s\n" "$pacman_suggestions"
    fi
    if [ -n "$paru_suggestions" ]; then
        printf "%s\n" "$paru_suggestions"
    fi
}
