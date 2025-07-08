local M = {}
local opts = { noremap = true, silent = true }

local u = require("utils").update

M.general = {
  { "v", "<leader>y", '"+yg_', { desc = "Yank to clipboard" } },
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

  { { "n", "v" }, "<A-i>", ":ToggleTerm<CR>", opts },
  { { "i" }, "<A-i>", "<ESC>:ToggleTerm<CR>", opts },
  { { "t" }, "<A-i>", "<C-\\><C-n>:ToggleTerm<CR>", opts },

  {
    { "n", "v" },
    "<leader>wj",
    "<C-w>j",
    u(opts, { desc = "Move to pane below" }),
  },
  {
    { "n", "v" },
    "<leader>wk",
    "<C-w>k",
    u(opts, { desc = "Move to pane above" }),
  },
  {
    { "n", "v" },
    "<leader>wh",
    "<C-w>h",
    u(opts, { desc = "Move to pane left" }),
  },
  {
    { "n", "v" },
    "<leader>wl",
    "<C-w>l",
    u(opts, { desc = "Move to pane right" }),
  },
  {
    { "n", "v" },
    "<leader>wo",
    "<C-w>o",
    u(opts, { desc = "Keep the only pane" }),
  },
  {
    { "n" },
    "<leader>wv",
    ":vsp<CR>",
    u(opts, { desc = "Vertical split" }),
  },

  {
    { "n", "x" },
    "gw",
    "*N",
    u(opts, { desc = "Cycle through last search" }),
  },
  { { "n", "v" }, "s", "f", opts }, -- remap s -> f since f is taken by `leap`
}

return M
