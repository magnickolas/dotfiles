vim.pack.add({ "https://github.com/neovim/nvim-lspconfig" })

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(ev)
        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        if client == nil then return end

        local opts = { buffer = ev.buf, noremap = true, silent = true }
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
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

vim.lsp.enable({
    "lua_ls",
    "pyright",
    "ccls",
    "gopls",
})

local function on_jump(diagnostic, bufnr)
    if not diagnostic then return end
    vim.diagnostic.open_float(bufnr)
end

vim.diagnostic.config({ jump = { on_jump = on_jump } })
