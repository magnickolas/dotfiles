map = vim.api.nvim_set_keymap
opts = { noremap = true, silent = true }

-- Copy to clipboard
map('n', '<Leader>Y', '"+yg_', opts)

-- Paste from clipboard
map('n', '<Leader>p', '"+p', opts)
map('n', '<Leader>P', '"+P', opts)
map('v', '<Leader>p', '"+p', opts)
map('v', '<Leader>P', '"+P', opts)

-- Delete to clipboard
map('v', '<Leader>d', '"+d', opts)
map('n', '<Leader>d', '"+d', opts)
map('n', '<Leader>D', '"+D', opts)

-- Disable highlight
map('n', '<c-l>', ':noh<cr>', opts)

-- Use '0' key to switch between beginning of line and first column
map('n', '0', 'virtcol(".") == 1 ? "^" : "0"', { noremap = true, silent = true, expr = true })

-- Mouse support
vim.opt.mouse="a"

-- New tab
vim.cmd('ca tn tabnew')

-- Tabs
map('n', '<a-l>', ':tabnext<cr>', opts)
map('n', 'tl', ':tabnext<cr>', opts)
map('n', '<a-h>', ':tabprev<cr>', opts)
map('n', 'th', ':tabprev<cr>', opts)
map('n', '<a-L>', ':tabm +1<cr>', { silent=true })
map('n', '<a-H>', ':tabm -1<cr>', { silent=true })

-- Terminal enter normal mode
map('t', '<esc>', '<c-\\><c-n>', opts)

-- Shortcuts for plugins
require('telescope-shortcuts')
require('easyalign-shortcuts')
require('lspconfig-shortcuts')
require('oscyank-shortcuts')
require('ctrlsf-shortcuts')
require('vsnip-shortcuts')
