local M = {}

M.load_mappings = function(m)
    for _, mappings in pairs(m) do
        for _, mapping in pairs(mappings) do
            vim.keymap.set(
                mapping[1],
                mapping[2],
                mapping[3],
                mapping[4]
            )
        end
    end
end

M.set_colorscheme = function()
    vim.g.vscode_style = "dark"
    vim.cmd[[colorscheme vscode]]
end

M.configure_backup = function()
    local undo_dir = vim.fn.expand("~/.vim/undo")
    if vim.fn.isdirectory(undo_dir) == 0 then
        vim.fn.mkdir(undo_dir, "p")
    end
    vim.opt.undodir     = undo_dir
    vim.opt.undofile    = true
    vim.opt.writebackup = false
    vim.opt.backup      = false
end



return M
