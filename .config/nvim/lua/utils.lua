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
    vim.cmd [[colorscheme tokyonight-night]]
    if not vim.g.neovide then
        vim.cmd [[:hi! Normal ctermbg=NONE guibg=NONE<CR>]]
    end
    vim.cmd [[highlight CursorLine cterm=NONE ctermbg=NONE ctermfg=NONE guibg=NONE guifg=NONE]]
    vim.o.cursorline = true
end

M.setup_neovide = function()
    if not vim.g.neovide then
        return
    end
    vim.g.neovide_transparency = 0.93
    vim.g.neovide_cursor_animation_length = 0.04

    RefreshGuiFont = function()
        vim.opt.guifont = string.format("%s:h%s", GUI_FONT_FACE, GUI_FONT_SIZE)
    end

    ResizeGuiFont = function(delta)
        GUI_FONT_SIZE = GUI_FONT_SIZE + delta
        RefreshGuiFont()
    end

    ResetGuiFont = function()
        GUI_FONT_SIZE = 18
        GUI_FONT_FACE = "Iosevka Nerd Font Mono"
        RefreshGuiFont()
    end

    -- Call function on startup to set default value
    ResetGuiFont()

    -- Keymaps
    local opts = { noremap = true, silent = true }

    vim.keymap.set({ 'n', 'i' }, "<C-=>", function() ResizeGuiFont(1) end, opts)
    vim.keymap.set({ 'n', 'i' }, "<C-->", function() ResizeGuiFont(-1) end, opts)
    vim.keymap.set({ 'n', 'i' }, "<C-0>", function() ResetGuiFont() end, opts)
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

M.init_screen = function()
    --vim.opt.shortmess:append({ I = true })
end

M.configure_editor = function()
    local function paste_keep_cursor_column(above)
        local col = vim.fn.virtcol(".")
        vim.cmd(string.format("normal! %s", above and "P" or "p"))
        if vim.fn.getregtype() == "V" then
            vim.fn.cursor(".", col)
        end
    end

    vim.keymap.set('n', "p", function() paste_keep_cursor_column(false) end, { noremap = true, silent = true })
    vim.keymap.set('n', "P", function() paste_keep_cursor_column(true) end, { noremap = true, silent = true })

    vim.o.startofline    = false
    vim.o.tabstop        = 4
    vim.o.shiftwidth     = 4
    vim.o.expandtab      = true
    vim.o.autoindent     = false
    vim.o.smartindent    = false
    vim.o.ignorecase     = false
    vim.o.number         = true
    vim.o.hidden         = true
    vim.o.completeopt    = 'menuone,noinsert,noselect'
    vim.o.showmode       = false
    vim.o.foldmethod     = 'marker'
    vim.o.foldlevel      = 0
    vim.o.autochdir      = false
    vim.o.splitright     = true
    vim.o.splitbelow     = true
    vim.o.updatetime     = 750
    vim.o.mouse          = 'a'
    -- vim.opt.list = true
    -- vim.cmd [[
    --     highlight WhiteSpaceAol guifg=#f7768e
    --     highlight WhiteSpaceMol guifg=#1a1b26
    --     match WhiteSpaceMol / /
    --     2match WhiteSpaceAol / \+$/
    -- ]]
    vim.o.relativenumber = true
    if vim.fn.executable("rg") then
        -- if ripgrep installed, use that as a grepper
        vim.opt.grepprg = "rg --vimgrep --no-heading"
        vim.opt.grepformat = "%f:%l:%c:%m,%f:%l:%m"
    end

    vim.cmd [[ca tn tabnew]]


end

return M
