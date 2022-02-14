local lspconfig  = require('lspconfig')
local lsp_installer = require("nvim-lsp-installer")

lsp_installer.on_server_ready(function(server)
    local opts = {}

    server:setup(opts)
end)

hls_config = lspconfig["hls"]
if hls_config ~= nil then
    hls_config.setup{
        settings = {
          haskell = {
            formattingProvider = "fourmolu"
          }
        }
    }
end
