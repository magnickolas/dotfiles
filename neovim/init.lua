vim.g.mapleader = "\\"
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
require('lspconfig-config')
require('nvim-compe-config')
require('nvim-comment-config')
require('rust-tools-config')
