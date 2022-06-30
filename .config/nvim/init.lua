vim.g.mapleader = " "
vim.opt.background = "dark"
vim.o.shell = "/bin/zsh"
require('plugins')
local utils = require('utils')
utils.load_mappings(require('mappings'))
utils.set_colorscheme()
utils.configure_backup()
utils.init_screen()
utils.configure_editor()
