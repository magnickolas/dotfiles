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
        -- ['<C-p>']     = cmp.mapping.select_prev_item(),
        -- ['<C-n>']     = cmp.mapping.select_next_item(),
        ["<C-n>"] = function(fallback)
            if cmp.visible() then
                return cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert }(fallback)
            else
                return cmp.mapping.complete { reason = cmp.ContextReason.Auto }(fallback)
            end
        end,
        ["<C-p>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },
        ['<C-d>']     = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
        ['<C-f>']     = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
        ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
        ['<C-y>']     = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
        ['<C-e>']     = cmp.mapping({
            i         = cmp.mapping.abort(),
            c         = cmp.mapping.close(),
        }),
        ['<CR>']      = cmp.mapping.confirm({ select = false }),
        -- ["<CR>"] = function(fallback)
        --     if cmp.visible() then
        --         return cmp.mapping.confirm {
        --             behavior = cmp.ConfirmBehavior.Insert,
        --             select = true,
        --         }(fallback)
        --     else
        --         return fallback()
        --     end
        -- end,
        -- ['<Tab>']     = cmp.mapping(function(fallback)
        --     if require('luasnip').expand_or_jumpable() then
        --         vim.fn.feedkeys(vim.api.nvim_replace_termcodes('<Plug>luasnip-expand-or-jump', true, true, true), '')
        --     elseif cmp.visible() then
        --         cmp.select_next_item()
        --     else
        --         fallback()
        --     end
        -- end, {
        --     'i',
        --     's',
        -- }),
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
        { name = "path", priority_weight = 110 },
        { name = "orgmode", priority_weight = 110 },
        { name = "nvim_lsp", max_item_count = 20, priority_weight = 100 },
        { name = "nvim_lua", priority_weight = 90 },
        { name = "luasnip", priority_weight = 80 },
        { name = "buffer", max_item_count = 5, priority_weight = 70 },
        {
            name = "rg",
            keyword_length = 5,
            max_item_count = 5,
            priority_weight = 60,
            option = {
                additional_arguments = "--smart-case --hidden",
            },
        },
        { name = "tmux", max_item_count = 5, option = { all_panes = false }, priority_weight = 50 },
        {
            name = "look",
            keyword_length = 5,
            max_item_count = 5,
            option = { convert_case = true, loud = true },
            priority_weight = 40,
        },
    },
    window = {
        documentation = {
            border = vim.g.floating_window_border_dark,
        },
        completion = {
            border = vim.g.floating_window_border_dark,
        },
    },

    experimental = {
        ghost_text = true,
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
