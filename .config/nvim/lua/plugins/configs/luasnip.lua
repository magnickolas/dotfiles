require('luasnip.loaders.from_vscode').lazy_load(
    { paths = vim.fn.expand('$HOME/.config/nvim/snippets') }
)
