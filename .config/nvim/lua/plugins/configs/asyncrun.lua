vim.g.asyncrun_open = 6

vim.api.nvim_exec([[
function! s:alacritty_runner(opts)
    let cmds = []
    let cmds += ['cd ' . shellescape(getcwd()) ]
    let cmds += [a:opts.cmd]
    let cmds += ['echo ""']
    let cmds += ['read -n1 -rsp "press any key to continue ..."']
    let text = shellescape(join(cmds, ";"))
    let command = 'alacritty -e bash -c ' . text
    call system(command . ' &')
endfunction

let g:asyncrun_runner = get(g:, 'asyncrun_runner', {})
let g:asyncrun_runner.alacritty = function('s:alacritty_runner')
]], false)
