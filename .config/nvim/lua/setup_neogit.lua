local neogit = require "neogit"

neogit.setup({
    highlight = {
        bg_green = "#d5c4a1",
        line_green = "NONE",
        green = "#7fd36b",
        inline_green = "#2f5a46",
        bg_red = "#d5c4a1",
        line_red = "NONE",
        red = "#ff6f7a",
        inline_red = "#7f2633",
    },
})

local function apply_neogit_diff_highlights()
    local set_hl = vim.api.nvim_set_hl
    set_hl(0, "NeogitDiffContext", { bg = "NONE" })
    set_hl(0, "NeogitDiffContextHighlight", { bg = "NONE" })
    set_hl(0, "NeogitDiffContextCursor", { bg = "NONE" })
    set_hl(0, "NeogitDiffAdd", { fg = "#7fd36b", bg = "NONE" })
    set_hl(0, "NeogitDiffAddHighlight", { fg = "#7fd36b", bg = "NONE" })
    set_hl(0, "NeogitDiffAddInline", { fg = "#fbf1c7", bg = "#2f5a46", bold = true })
    set_hl(0, "NeogitDiffDelete", { fg = "#ff6f7a", bg = "NONE" })
    set_hl(0, "NeogitDiffDeleteHighlight", { fg = "#ff6f7a", bg = "NONE" })
    set_hl(0, "NeogitDiffDeleteInline", { fg = "#fbf1c7", bg = "#7f2633", bold = true })
end

apply_neogit_diff_highlights()
vim.api.nvim_create_autocmd("ColorScheme", {
    callback = apply_neogit_diff_highlights,
})

return neogit
