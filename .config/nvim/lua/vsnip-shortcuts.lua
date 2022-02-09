
-- map('n', 'ga', "<Plug>(EasyAlign)", {})
-- map('v', 'ga', "<Plug>(EasyAlign)", {})
-- map('x', 'ga', "<Plug>(EasyAlign)", {})
local vsnip_opts = { expr = true }

-- Expand
map('i', '<c-j>', 'vsnip#expandable() ? "<Plug>(vsnip-expand)" : "<c-j>"', vsnip_opts)
map('s', '<c-j>', 'vsnip#expandable() ? "<Plug>(vsnip-expand)" : "<c-j>"', vsnip_opts)

-- Expand or jump
map('i', '<c-l>', 'vsnip#available(1) ? "<Plug>(vsnip-expand-or-jump)" : "<c-l>"', vsnip_opts)
map('s', '<c-l>', 'vsnip#available(1) ? "<Plug>(vsnip-expand-or-jump)" : "<c-l>"', vsnip_opts)

-- Jump forward or backward
map('i', '<tab>', 'vsnip#jumpable(1) ? "<Plug>(vsnip-jump-next)" : "<tab>"', vsnip_opts)
map('s', '<tab>', 'vsnip#jumpable(1) ? "<Plug>(vsnip-jump-next)" : "<tab>"', vsnip_opts)
map('i', '<s-tab>', 'vsnip#jumpable(-1) ? "<Plug>(vsnip-jump-prev)" : "<s-tab>"', vsnip_opts)
map('s', '<s-tab>', 'vsnip#jumpable(-1) ? "<Plug>(vsnip-jump-prev)" : "<s-tab>"', vsnip_opts)

-- Select or cut text to use as $TM_SELECTED_TEXT in the next snippet.
-- See https://github.com/hrsh7th/vim-vsnip/pull/50
map('n', 's', '<Plug>(vsnip-select-text)', {})
map('x', 's', '<Plug>(vsnip-select-text)', {})
map('n', 'S', '<Plug>(vsnip-cut-text)', {})
map('x', 'S', '<Plug>(vsnip-cut-text)', {})
