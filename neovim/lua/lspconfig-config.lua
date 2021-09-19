local lspconfig = require('lspconfig')
require'lspinstall'.setup()

local servers = require'lspinstall'.installed_servers()
for _, server in pairs(servers) do
  require'lspconfig'[server].setup{}
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
