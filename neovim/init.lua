vim.g.mapleader = "\\"
vim.opt.background = "dark"
vim.o.shell = "/bin/zsh"
require('plugins')
require('shortcuts')
require('colorscheme')
require('undo-backup')
require('initscreen')
require('editor')

-- Plugins configs
require('lualine-config')
require('toggleterm-config')
require('gutentags-config')
require('nvim-cmp-config')
require('lspconfig-config')
require('nvim-comment-config')
require('rust-tools-config')
require('oscyank-config')
require('orgmode-config')
require('vim-autosource-config')
