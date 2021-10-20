local lspconfig  = require('lspconfig')
local lspinstall = require('lspinstall')
lspinstall.setup()

local function setup_servers()
  lspinstall.setup()
  local servers = lspinstall.installed_servers()
  for _, server in pairs(servers) do
    lspconfig[server].setup{capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())}
  end
end

setup_servers()

-- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
lspinstall.post_install_hook = function ()
  setup_servers() -- reload installed servers
  vim.cmd("bufdo e") -- this triggers the FileType autocmd that starts the server
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
