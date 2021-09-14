local lspconfig = require('lspconfig')
lspconfig.pyright.setup{}
lspconfig.rust_analyzer.setup{}
lspconfig.ccls.setup{}
lspconfig.cmake.setup{}
--require('lspconfig').rls.setup{}
--nvim_lsp.rls.setup {
--  settings = {
--    rust = {
--      unstable_features = true,
--      build_on_save = false,
--      all_features = true,
--    },
--  },
--}
