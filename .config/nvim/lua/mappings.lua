local M = {}
local opts = { noremap = true, silent = true }

local function paste_keep_cursor_column(above)
  return function()
    vim.cmd("normal! " .. (above and "P" or "p") .. vim.fn.col(".") .. "|")
  end
end

local u = require("utils").update

M.general = {
  { { "n" }, "p", paste_keep_cursor_column(false), opts },
  { { "n" }, "P", paste_keep_cursor_column(true), opts },
  { "v", "<leader>y", '"+yg_', { desc = "Yank to clipboard" } },
  { { "n", "v" }, "<leader>p", '"+p', { desc = "Paste from clipboard" } },
  { { "n", "v" }, "<leader>P", '"+P', { desc = "Paste from clipboard" } },
  { { "n", "v" }, "<leader>d", '"+d', { desc = "Delete to clipboard" } },
  { "n", "<leader>D", '"+D', { desc = "Delete to clipboard" } },
  -- Don't move cursor after visual yank
  -- { "v", "y", "ygv<esc>", {} },
  -- Disable highlight
  { "n", "<esc>", ":noh<cr>", opts },
  -- Use '0' key to switch between beginning of line and first column
  { "n", "0", 'virtcol(".")== 1 ? "^" : "0"', { expr = true } },
  -- Tabs
  { "n", "<a-l>", ":tabnext<cr>", opts },
  { "n", "tl", ":tabnext<cr>", opts },
  { "n", "<a-h>", ":tabprev<cr>", opts },
  { "n", "th", ":tabprev<cr>", opts },
  { "n", "<a-L>", ":tabm +1<cr>", opts },
  { "n", "<a-H>", ":tabm -1<cr>", opts },
  -- Move to next window
  { "n", "<c-l>", ":wincmd w<cr>", opts },
  -- Move to previous window
  { "n", "<c-h>", ":wincmd W<cr>", opts },
  -- Terminal enter normal mode
  { "t", "<esc>", "<c-\\><c-n>", opts },
  -- Replace for ← → in insert mode
  { { "i" }, "<c-l>", "<right>", opts },
  { { "i" }, "<c-h>", "<left>", opts },
  { { "i" }, "<a-h>", "<C-\\><C-O>b", opts },
  { { "i" }, "<a-l>", "<C-\\><C-O>w", opts },
  -- Switch panes
  { { "n" }, "<left>", "<C-w>h", opts },
  { { "n" }, "<down>", "<C-w>j", opts },
  { { "n" }, "<up>", "<C-w>k", opts },
  { { "n" }, "<right>", "<C-w>l", opts },
  -- Resize panes
  { { "n" }, "<S-Up>", "<cmd>resize +2<CR>", opts },
  { { "n" }, "<S-Down>", "<cmd>resize -2<CR>", opts },
  { { "n" }, "<S-Left>", "<cmd>vertical resize -2<CR>", opts },
  { { "n" }, "<S-Right>", "<cmd>vertical resize +2<CR>", opts },

  { { "n", "v" }, "<leader>wj", "<C-w>j", u(opts, { desc = "Move to pane below" }) },
  { { "n", "v" }, "<leader>wk", "<C-w>k", u(opts, { desc = "Move to pane above" }) },
  { { "n", "v" }, "<leader>wh", "<C-w>h", u(opts, { desc = "Move to pane left" }) },
  { { "n", "v" }, "<leader>wl", "<C-w>l", u(opts, { desc = "Move to pane right" }) },

  { { "n", "x" }, "gw", "*N", u(opts, { desc = "Cycle through last search" }) },
  { { "n", "v" }, "s", "f", opts }, -- remap s -> f since f is taken by `leap`

  { { "n" }, "<leader>tf", require("plugins.lsp.format").toggle, { desc = "Toggle format on Save" } },
}

return M
