local has_lsp_installer, lsp_installer = pcall(require, 'nvim-lsp-installer')
local servers = {}
if has_lsp_installer then
    lsp_installer.setup{}
    servers = lsp_installer.get_installed_servers()
end
local lspconfig = require('lspconfig')
local has_capabilities, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
local capabilities = nil
if has_capabilities then
    capabilities = cmp_nvim_lsp.default_capabilities(
        vim.lsp.protocol.make_client_capabilities()
    )
end

for _, server in ipairs(servers) do
    local config = lspconfig[server.name]
    if config ~= nil then
        if server.name == "sumneko_lua" then
            config.setup {
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
        elseif server.name == "rust_analyzer" then
            config.setup {
                settings = { },
                --cmd = { "ra-multiplex" },
                capabilities = capabilities,
            }
        elseif server.name == "hls" then
            config.setup {
                settings = {
                    haskell = {
                        formattingProvider = 'fourmolu'
                    }
                },
                capabilities = capabilities,
            }
        else
            config.setup {
                settings = {},
                capabilities = capabilities,
            }
        end
    end
end

-- local black = require "efm/black"
-- local isort = require "efm/isort"
-- local flake8 = require "efm/flake8"
-- local mypy = require "efm/mypy"

-- lspconfig.efm.setup {
--     capabilities = capabilities,
--     init_options = { documentFormatting = true },
--     root_dir = vim.loop.cwd,
--     settings = {
--         rootMarkers = { ".git/" },
--         lintDebounce = 100,
--         -- logLevel = 5,
--         languages = {
--             python = { black, isort, flake8, mypy },
--         },
--     },
-- }
