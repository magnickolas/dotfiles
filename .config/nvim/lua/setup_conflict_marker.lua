vim.g.conflict_marker_highlight_group = ""
vim.g.conflict_marker_begin = "^<<<<<<<\\+ .*$"
vim.g.conflict_marker_common_ancestors = "^|||||||\\+ .*$"
vim.g.conflict_marker_end = "^>>>>>>>\\+ .*$"

local function apply_conflict_marker_highlights()
    local set_hl = vim.api.nvim_set_hl

    set_hl(0, "ConflictMarkerBegin", { fg = "#fbf1c7", bg = "#8a5a00", bold = true })
    set_hl(0, "ConflictMarkerOurs", { bg = "#5a3b00" })
    set_hl(0, "ConflictMarkerCommonAncestors", { fg = "#f9e4ff", bg = "#7a2f5e", bold = true })
    set_hl(0, "ConflictMarkerCommonAncestorsHunk", { bg = "#4d243e" })
    set_hl(0, "ConflictMarkerSeparator", { fg = "#ebdbb2", bg = "#5b5350", bold = true })
    set_hl(0, "ConflictMarkerTheirs", { bg = "#005a66" })
    set_hl(0, "ConflictMarkerEnd", { fg = "#e6fcff", bg = "#007f8c", bold = true })
end

vim.api.nvim_create_autocmd("ColorScheme", {
    group = vim.api.nvim_create_augroup("ConflictMarkerColors", { clear = true }),
    callback = apply_conflict_marker_highlights,
})

apply_conflict_marker_highlights()
