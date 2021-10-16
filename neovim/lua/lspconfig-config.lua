local lspconfig  = require('lspconfig')
local lspinstall = require('lspinstall')
lspinstall.setup()

local servers = lspinstall.installed_servers()
for _, server in pairs(servers) do
  lspconfig[server].setup{capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())}
end 
--lspconfig.pyright.setup{}
--lspconfig.rust_analyzer.setup{
--    settings = {
--        ["rust-analyzer"] = { 
--            checkOnSave = {
--                command = "clippy"
--            } 
--        } 
--    } 
--}
--lspconfig.ccls.setup{}
--lspconfig.cmake.setup{}
