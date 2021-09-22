map = vim.api.nvim_set_keymap
opts = { noremap = true, silent = true }

-- Copy to clipboard
map('v', '<Leader>y', '"+y', opts)
map('n', '<Leader>y', '"+y', opts)
map('n', '<Leader>Y', '"+yg_', opts)

-- Paste from clipboard
map('n', '<Leader>p', '"+p', opts)
map('n', '<Leader>P', '"+P', opts)

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

-- Shortcuts for plugins
require('telescope-shortcuts')
require('easyalign-shortcuts')
require('barbar-shortcuts')
require('lspconfig-shortcuts')
