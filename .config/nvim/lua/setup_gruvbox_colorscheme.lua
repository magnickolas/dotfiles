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
    inverse = true,
    contrast = "",
    palette_overrides = {},
    overrides = {
        StatusLine = { bg = "NONE" },
        StatusLineNC = { bg = "NONE" },
        StatusLineTerm = { bg = "NONE" },
        StatusLineTermNC = { bg = "NONE" },
        DiffAdd = { fg = "#7fd36b", bg = "NONE" },
        DiffDelete = { fg = "#ff6f7a", bg = "NONE" },
        DiffChange = { fg = "#d5c4a1", bg = "#2b3338" },
        DiffText = { fg = "#fbf1c7", bg = "#4c3c66" },
    },
    dim_inactive = false,
    transparent_mode = true,
})
vim.cmd("colorscheme gruvbox")
