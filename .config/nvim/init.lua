vim.g.mapleader = " "
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.expandtab = true
vim.o.wrap = true
vim.o.winborder = "rounded"
vim.o.signcolumn = "yes"
local undo_dir = vim.fn.expand("~/.vim/undo")
if vim.fn.isdirectory(undo_dir) == 0 then
    vim.fn.mkdir(undo_dir, "p")
end
vim.o.undodir = undo_dir
vim.o.undofile = true
vim.o.backup = true
vim.o.backupdir = vim.fn.stdpath("state") .. "/backup"
vim.o.swapfile = false
vim.g.c_no_curly_error = 1

require "setup_conflict_marker"

local gh = "https://github.com/"
vim.pack.add({
    gh .. "stevearc/oil.nvim",
    gh .. "echasnovski/mini.pick",
    gh .. "echasnovski/mini.extra",
    gh .. "echasnovski/mini.misc",
    gh .. "numToStr/Comment.nvim",
    gh .. "tpope/vim-fugitive",
    gh .. "mrcjkb/rustaceanvim",
    gh .. "saecki/crates.nvim",
    gh .. "mg979/vim-visual-multi",
    "https://codeberg.org/andyg/leap.nvim",
    { src=gh .. "Saghen/blink.cmp", version="v1.9.1" },
    gh .. "nvim-treesitter/nvim-treesitter",
    gh .. "dyng/ctrlsf.vim",
    gh .. "lewis6991/gitsigns.nvim",
    gh .. "1A7432/nvim-python-venv",
    gh .. "rhysd/conflict-marker.vim",
    gh .. "simnalamburt/vim-mundo",
})

require "nvim-python-venv".setup({
    auto_detect = true,
    auto_activate = false,
    auto_restart_lsp = false,
    ui = {
        notify = false,
    },
})
require "setup_gruvbox_colorscheme"
require "setup_lualine"
require "setup_mundo"
require "lsp"
require "oil".setup()
vim.keymap.set({ "n", "x", "o" }, "s", "<Plug>(leap)", { silent = true, desc = "Leap current window" })
vim.keymap.set("n", "S", "<Plug>(leap-from-window)", { silent = true, desc = "Leap from window" })
vim.keymap.set({ "x", "o" }, "x", "<Plug>(leap-forward-till)", { silent = true, desc = "Leap forward till" })
vim.keymap.set({ "x", "o" }, "X", "<Plug>(leap-backward-till)", { silent = true, desc = "Leap backward till" })
vim.keymap.set({ "n", "x", "o" }, "gs", "<Plug>(leap-forward)", { silent = true, desc = "Leap forward" })
vim.keymap.set({ "n", "x", "o" }, "gS", "<Plug>(leap-backward)", { silent = true, desc = "Leap backward" })
require "blink.cmp".setup({
    keymap = {
      preset = "default",
      ["<c-a>"] = { "select_and_accept" },
    },
})
require("nvim-treesitter").setup({})
vim.api.nvim_create_autocmd("FileType", {
    pattern = { "pony" },
    callback = function()
        pcall(vim.treesitter.start)
    end,
})
require "crates".setup{}
require "gitsigns".setup{}

require "mini.pick".setup({
    window = {
        config = function()
            return {
                width = vim.o.columns,
            }
        end,
    },
})
require "mini.extra".setup()
require "mini.misc".setup()
MiniMisc.setup_auto_root()
MiniMisc.setup_restore_cursor()
require "Comment".setup({
    opleader = {
        line = "<leader>/",
        block = "<leader>*",
    },
})

---@param lhs string           Left-hand side |{lhs}| of the mapping.
---@param rhs string|function  Right-hand side |{rhs}| of the mapping, can be a Lua function.
---@param opts? vim.keymap.set.Opts
local function nmap(lhs, rhs, opts)
    vim.keymap.set("n", lhs, rhs, opts)
end

vim.keymap.set("v", "<leader>y", '"+yg_')
vim.keymap.set("v", "<leader>d", '"+yg_')
vim.keymap.set({ "n", "v" }, "<leader>p", '"+p')
vim.keymap.set("t", "<ESC>", "<C-\\><C-n>")

vim.keymap.set({ "i" }, "<c-l>", "<right>")
vim.keymap.set({ "i" }, "<c-h>", "<left>")
vim.keymap.set({ "i" }, "<a-h>", "<C-\\><C-O>b")
vim.keymap.set({ "i" }, "<a-l>", "<C-\\><C-O>w")

vim.cmd([[ca tn tabnew]])
nmap("tl", ":tabnext<CR>")
nmap("th", ":tabprev<CR>")
nmap("tn", ":tabnew<CR>")
nmap("<a-l>", ":tabnext<cr>")
nmap("<a-h>", ":tabprev<cr>")
nmap("<a-L>", ":tabm +1<cr>")
nmap("<a-H>", ":tabm -1<cr>")
nmap("<c-l>", ":wincmd w<cr>")
nmap("<c-h>", ":wincmd W<cr>")
nmap("<esc>", ":noh<cr>")
nmap("]w", function() vim.diagnostic.jump({ severity = vim.diagnostic.severity.WARN, count = 1 }) end)
nmap("[w", function() vim.diagnostic.jump({ severity = vim.diagnostic.severity.WARN, count = -1 }) end)
nmap("]e", function() vim.diagnostic.jump({ severity = vim.diagnostic.severity.ERROR, count = 1 }) end)
nmap("[e", function() vim.diagnostic.jump({ severity = vim.diagnostic.severity.ERROR, count = -1 }) end)
nmap("<leader>cf", vim.lsp.buf.format)
nmap("<leader>ff", MiniPick.builtin.files)
nmap("<leader>b", MiniPick.builtin.buffers)
nmap("<leader>fs", function() MiniExtra.pickers.lsp({ scope = 'document_symbol' }) end)
nmap("<leader>/", function() MiniExtra.pickers.buf_lines({ scope = 'current' }) end)
nmap("<leader>fg", MiniPick.builtin.grep_live)
nmap("<leader>h", MiniPick.builtin.help)
nmap("<leader>?", function() MiniPick.start({ source = { items = vim.v.oldfiles } }) end)
nmap("<leader>z", require "mini.misc".zoom)
nmap("<leader>sf", ":CtrlSF ")
nmap("<leader>u", "<cmd>MundoToggle<CR>")

vim.api.nvim_create_autocmd("FileType", {
    pattern = "json",
    callback = function(ev)
      nmap("%", "%", { buffer = ev.buf, remap = false, silent = true })
    end,
  })
