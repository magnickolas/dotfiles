-- Setup nvim-cmp.
local cmp = require 'cmp'

cmp.setup({
    snippet = {
        -- REQUIRED - you must specify a snippet engine
        expand = function(args)
            require('luasnip').lsp_expand(args.body)
        end,
    },
    mapping = {
        ['<C-p>']     = cmp.mapping.select_prev_item(),
        ['<C-n>']     = cmp.mapping.select_next_item(),
        ['<C-d>']     = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
        ['<C-f>']     = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
        ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
        ['<C-y>']     = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
        ['<C-e>']     = cmp.mapping({
            i         = cmp.mapping.abort(),
            c         = cmp.mapping.close(),
        }),
        ['<CR>']      = cmp.mapping.confirm({ select = false }),
        ['<Tab>']     = cmp.mapping(function(fallback)
            if require('luasnip').expand_or_jumpable() then
                vim.fn.feedkeys(vim.api.nvim_replace_termcodes('<Plug>luasnip-expand-or-jump', true, true, true), '')
            elseif cmp.visible() then
                cmp.select_next_item()
            else
                fallback()
            end
        end, {
            'i',
            's',
        }),
        ['<S-Tab>']   = cmp.mapping(function(fallback)
            if require('luasnip').jumpable(-1) then
                vim.fn.feedkeys(vim.api.nvim_replace_termcodes('<Plug>luasnip-jump-prev', true, true, true), '')
            elseif cmp.visible() then
                cmp.select_prev_item()
            else
                fallback()
            end
        end, {
            'i',
            's',
        }),
    },
    sources = {
        { name = 'luasnip' },
        { name = 'nvim_lsp' },
        { name = 'nvim_lua' },
    },
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
    sources = {
        { name = 'buffer' }
    }
})

-- -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        { name = 'cmdline' }
    })
})
