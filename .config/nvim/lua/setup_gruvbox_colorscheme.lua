vim.pack.add({ "https://github.com/ellisonleao/gruvbox.nvim" })
require("gruvbox").setup({
    undercurl = true,
    underline = true,
    bold = true,
    italic = { strings = true, operators = true, comments = true },
    strikethrough = true,
    invert_selection = false,
    invert_signs = false,
    invert_tabline = false,
    invert_intend_guides = false,
    inverse = true,
    contrast = "",
    palette_overrides = {},
    overrides = {},
    dim_inactive = false,
    transparent_mode = true,
})
vim.cmd("colorscheme gruvbox")
