local M = {
  "mrcjkb/rustaceanvim",
  ft = "rust",
  version = "^4",
  lazy = false,
  config = function()
    vim.g.rustaceanvim = {
      server = {
        cmd = { "ra-multiplex", "client" },
      },
    }
  end,
  -- config = function()
  -- local rt = require("rust-tools")
  -- local extension_path = vim.env.HOME .. "/.config/nvim/dap/codelldb/extension"
  -- local codelldb_path = extension_path .. "/adapter/codelldb"
  -- local liblldb_path = extension_path .. "/lldb/lib/liblldb.so"
  -- rt.setup({
  --     tools = {
  --         autoSetHints = true,
  --         runnables = {
  --             use_telescope = true,
  --         },
  --         inlay_hints = {
  --             show_parameter_hints = true,
  --             parameter_hints_prefix = "",
  --             other_hints_prefix = "",
  --         },
  --     },
  --     dap = {
  --         adapter = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path),
  --     },
  --     server = {
  --         -- cmd = { "ra-multiplex", "client" },
  --         on_attach = function(_, bufnr)
  --             vim.keymap.set("n", "<C-k>", rt.hover_actions.hover_actions, { buffer = bufnr })
  --             vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
  --             vim.keymap.set({ "n", "v" }, "<a-c>", ":RustRunnables<CR>", { buffer = bufnr })
  --             vim.keymap.set({ "n", "v" }, "<F5>", ":RustDebuggables<CR>", { buffer = bufnr })
  --             vim.api.nvim_create_autocmd({ "BufEnter" }, {
  --                 pattern = "*.rs",
  --                 callback = function()
  --                     rt.inlay_hints.enable()
  --                 end,
  --             })
  --         end,
  --         settings = {
  --             ["rust-analyzer"] = {
  --                 checkOnSave = {
  --                     command = "clippy",
  --                 },
  --             },
  --         },
  --     },
  -- })
  -- end,
}

return M
