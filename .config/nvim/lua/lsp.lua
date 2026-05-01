vim.pack.add({ "https://github.com/neovim/nvim-lspconfig" })

local locations_to_items = vim.lsp.util.locations_to_items
function vim.lsp.util.locations_to_items(locations, position_encoding)
    if locations == nil then
        return {}
    end
    return locations_to_items(locations, position_encoding)
end

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(ev)
        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        if client == nil then return end

        local opts = { buffer = ev.buf, noremap = true, silent = true }
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        if client.name == "rust-analyzer" then
            if client:supports_method("textDocument/inlayHint") then
                vim.lsp.inlay_hint.enable(false, { bufnr = ev.buf })
                vim.keymap.set('n', '<leader>th', function()
                    local enabled = vim.lsp.inlay_hint.is_enabled({ bufnr = ev.buf })
                    vim.lsp.inlay_hint.enable(not enabled, { bufnr = ev.buf })
                end, vim.tbl_extend('force', opts, { desc = 'Toggle inlay hints' }))
            end
            vim.keymap.set('n', 'K', function()
                vim.cmd.RustLsp({ 'hover', 'actions' })
            end, opts)
        else
            vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        end
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
        vim.keymap.set('n', '<leader>r', vim.lsp.buf.rename, opts)
        vim.keymap.set('n', '<leader>a', vim.lsp.buf.code_action, opts)
    end,
})
vim.cmd("set completeopt+=noselect")

vim.lsp.config("lua_ls", {
    settings = {
        Lua = {
            workspace = {
                library = vim.api.nvim_get_runtime_file("", true),
                checkThirdParty = false,
            },
        },
    },
})

vim.lsp.config("basedpyright", {
    on_attach = function(client, bufnr)
        local root_dir = client.config.root_dir
        if root_dir == nil then
            local bufname = vim.api.nvim_buf_get_name(bufnr)
            root_dir = bufname ~= "" and vim.fs.dirname(bufname) or vim.uv.cwd()
        end

        local venv_path = require('nvim-python-venv.venv').detect(
            root_dir,
            require('nvim-python-venv.config').get()
        )
        if venv_path then
            client.config.settings = client.config.settings or {}
            client.config.settings.python = client.config.settings.python or {}
            client.config.settings.python.pythonPath = require('nvim-python-venv.common.os').get_python_executable(venv_path)
        end
    end
})

vim.lsp.enable({
    "lua_ls",
    "basedpyright",
    -- "ccls",
    "clangd",
    "gopls",
})

local function on_jump(diagnostic, bufnr)
    if not diagnostic then return end
    vim.diagnostic.open_float(bufnr)
end

vim.diagnostic.config({ jump = { on_jump = on_jump } })
