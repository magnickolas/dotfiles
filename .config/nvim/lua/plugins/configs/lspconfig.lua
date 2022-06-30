require('nvim-lsp-installer').setup {}
local lspconfig = require('lspconfig')
local capabilities = require('cmp_nvim_lsp').update_capabilities(
    vim.lsp.protocol.make_client_capabilities()
)

for _, lsp in ipairs({ 'ccls', 'pyright' }) do
    local config = lspconfig[lsp]
    if config ~= nil then
        config.setup {
            settings = {},
            capabilities = capabilities,
        }
    end
end

local sumneko_lua = lspconfig['sumneko_lua']
if sumneko_lua ~= nil then
    sumneko_lua.setup {
        single_file_support = true,
        settings = {
            Lua = {
                diagnostics = {
                    globals = {
                        'vim', 'use'
                    }
                }
            }
        },
        capabilities = capabilities,
    }
end

local hls_config = lspconfig['hls']
if hls_config ~= nil then
    hls_config.setup {
        settings = {
            haskell = {
                formattingProvider = 'fourmolu'
            }
        },
        capabilities = capabilities,
    }
end
