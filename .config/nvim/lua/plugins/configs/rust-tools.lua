local rt = require("rust-tools")
-- Update this path
local extension_path = vim.env.HOME .. '/.config/nvim/dap/codelldb/extension'
local codelldb_path = extension_path .. '/adapter/codelldb'
local liblldb_path = extension_path .. '/lldb/lib/liblldb.so'

rt.setup {
    tools = {
        autoSetHints = true,
        runnables = {
            use_telescope = true
        },
        inlay_hints = {
            show_parameter_hints = true,
            parameter_hints_prefix = "",
            other_hints_prefix = "",
        },
    },

    dap = {
        adapter = require('rust-tools.dap').get_codelldb_adapter(
            codelldb_path, liblldb_path)
    },

    -- all the opts to send to nvim-lspconfig
    -- these override the defaults set by rust-tools.nvim
    -- see https://github.com/neovim/nvim-lspconfig/blob/master/CONFIG.md#rust_analyzer
    server = {
        -- on_attach is a callback called when the language server attachs to the buffer
        -- on_attach = on_attach,
        -- cmd = { "ra-multiplex" },
        on_attach = function(_, bufnr)
          -- Hover actions
          vim.keymap.set("n", "<C-k>", rt.hover_actions.hover_actions, { buffer = bufnr })
          -- Code action groups
          vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
          vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
          vim.keymap.set({"n", "v"}, "<a-c>", ":RustRunnables<CR>", { buffer = bufnr })
          vim.keymap.set({"n", "v"}, "<F5>", ":RustDebuggables<CR>", { buffer = bufnr })
        end,
        settings = {
            -- to enable rust-analyzer settings visit:
            -- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
            ['rust-analyzer'] = {
                -- enable clippy on save
                checkOnSave = {
                    command = 'clippy'
                },
            }
        }
    },
}
