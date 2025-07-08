vim.g.mapleader = " "
vim.g.maplocalleader = " "

require("config.lazy")

local utils = require("init_utils")
utils.init_screen()
utils.configure_editor()
utils.load_mappings(require("mappings"))
