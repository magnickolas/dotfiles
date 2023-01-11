vim.g.mapleader = " "
require("config.lazy")
local utils = require("utils")
utils.init_screen()
utils.configure_editor()
vim.api.nvim_create_autocmd("User", {
  pattern = "VeryLazy",
  callback = function()
    utils.configure_backup()
    utils.load_mappings(require("mappings"))
  end,
})
