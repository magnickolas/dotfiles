vim.g.conflict_marker_highlight_group = ""
vim.g.conflict_marker_begin = "^<<<<<<<\\+ .*$"
vim.g.conflict_marker_common_ancestors = "^|||||||\\+ .*$"
vim.g.conflict_marker_end = "^>>>>>>>\\+ .*$"

local syntax_groups = table.concat({
    "ConflictMarkerBegin",
    "ConflictMarkerOurs",
    "ConflictMarkerCommonAncestors",
    "ConflictMarkerCommonAncestorsHunk",
    "ConflictMarkerSeparator",
    "ConflictMarkerTheirs",
    "ConflictMarkerEnd",
}, " ")

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

vim.api.nvim_create_autocmd({ "Syntax", "BufWinEnter" }, {
    group = vim.api.nvim_create_augroup("ConflictMarkerRefresh", { clear = true }),
    callback = function(ev)
        if vim.g.loaded_conflict_marker ~= 1 then
            return
        end
        if vim.api.nvim_get_current_buf() ~= ev.buf then
            return
        end
        if vim.fn["conflict_marker#detect#markers"]() == 0 then
            return
        end

        vim.cmd("silent! syntax clear " .. syntax_groups)
        vim.api.nvim_exec_autocmds("BufReadPost", {
            group = "ConflictMarkerDetect",
            buffer = ev.buf,
            modeline = false,
        })
    end,
})

apply_conflict_marker_highlights()
