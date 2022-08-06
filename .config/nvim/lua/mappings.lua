local M = {}
local opts = { noremap = true, silent = true }

M.general = {
    -- Copy to clipboard
    { 'v',        '<leader>y', '"+yg_',                        {} },
    -- Paste from clipboard
    { {'n', 'v'}, '<leader>p', '"+p',                          {} },
    { {'n', 'v'}, '<leader>P', '"+P',                          {} },
    -- Delete to clipboard
    { 'v',        '<leader>d', '"+d',                          {} },
    { 'n',        '<leader>d', '"+d',                          {} },
    { 'n',        '<leader>D', '"+D',                          {} },
    -- Disable highlight
    { 'n',        '<esc>',     ':noh<cr>',                     opts },
    -- Use '0' key to switch between beginning of line and first column
    { 'n',        '0',         'virtcol(".")== 1 ? "^" : "0"', { expr = true } },
    -- Tabs
    { 'n',        '<a-l>',     ':tabnext<cr>',                 opts },
    { 'n',        'tl',        ':tabnext<cr>',                 opts },
    { 'n',        '<a-h>',     ':tabprev<cr>',                 opts },
    { 'n',        'th',        ':tabprev<cr>',                 opts },
    { 'n',        '<a-L>',     ':tabm +1<cr>',                 opts },
    { 'n',        '<a-H>',     ':tabm -1<cr>',                 opts },
    -- Move to next window
    { 'n',        '<c-l>',     ':wincmd w<cr>',                opts },
    -- Move to previous window
    { 'n',        '<c-h>',     ':wincmd W<cr>',                opts },
    -- Terminal enter normal mode
    { 't',        '<esc>',     '<c-\\><c-n>',                  opts },
    -- Paste latest yanked text
    { {'n', 'x'}, ',p',        '"0p',                          opts },
}

M.ctrlsf = {
    { 'n', '<C-F>f', '<Plug>CtrlSFPrompt',     {} },
    { 'v', '<C-F>f', '<Plug>CtrlSFVwordPath',  {} },
    { 'v', '<C-F>F', '<Plug>CtrlSFVwordExec',  {} },
    { 'n', '<C-F>n', '<Plug>CtrlSFCwordPath',  {} },
    { 'n', '<C-F>p', '<Plug>CtrlSFPwordPath',  {} },
    { 'n', '<C-F>o', ':CtrlSFOpen<cr>',        { noremap = true } },
    { 'n', '<C-F>t', ':CtrlSFToggle<cr>',      { noremap = true } },
    { 'i', '<C-F>t', '<Esc>:CtrlSFToggle<cr>', { noremap = true } },
}

M.easyalign = {
    { 'n', 'ga', "<Plug>(EasyAlign)", {} },
    { 'v', 'ga', "<Plug>(EasyAlign)", {} },
    { 'x', 'ga', "<Plug>(EasyAlign)", {} },
}

M.lspconfig = {
    { 'n', '<Leader>rn', '<cmd>lua vim.lsp.buf.rename()<cr>',         opts },
    { 'n', 'gr',         '<cmd>lua vim.lsp.buf.references()<cr>',     opts },
    { 'n', 'gD',         '<cmd>lua vim.lsp.buf.declaration()<cr>',    opts },
    { 'n', 'gd',         '<cmd>lua vim.lsp.buf.definition()<cr>',     opts },
    { 'n', 'gi',         '<cmd>lua vim.lsp.buf.implementation()<cr>', opts },
    { 'n', 'gf',         '<cmd>lua vim.lsp.buf.formatting()<cr>',     opts },
    { 'n', 'K',          '<cmd>lua vim.lsp.buf.hover()<cr>',          opts },
    { 'n', '<C-k>',      '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts },
    { 'i', '<C-k>',      '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts },
    { 'n', '<Leader>a',  '<cmd>lua vim.lsp.buf.code_action()<cr>',    opts },
}

local nvterm_toggle = function() require("nvterm.terminal").toggle "float" end
M.nvterm = {
    { {'n', 't'}, '<A-i>', nvterm_toggle, opts },
    { {'n', 't'}, '<C-t>', nvterm_toggle, opts },
}

M.oscyank = {
    { 'v', '<Leader>y', ':OSCYank<cr>', opts },
    { 'n', '<Leader>y', ':OSCYank<cr>', opts },
}

local telescope = require 'telescope.builtin'
M.telescope = {
    { 'n', '<leader>ff', function() telescope.find_files() end, opts },
    { 'n', '<leader>fg', function() telescope.live_grep() end,  opts },
    { 'n', '<leader>fb', function() telescope.buffers() end,    opts },
    { 'n', '<leader>fh', function() telescope.help_tags() end,  opts },
}

M.nvimtree = {
    { 'n', '<C-c>', '<cmd>NvimTreeToggle<cr>', opts }
}

return M
