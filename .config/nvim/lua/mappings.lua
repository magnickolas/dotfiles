local M = {}
local opts = { noremap = true, silent = true }

M.general = {
    -- Copy to clipboard
    { 'v', '<leader>y', '"+yg_', {} },
    -- Paste from clipboard
    { { 'n', 'v' }, '<leader>p', '"+p', {} },
    { { 'n', 'v' }, '<leader>P', '"+P', {} },
    -- Delete to clipboard
    { 'v', '<leader>d', '"+d', {} },
    { 'n', '<leader>d', '"+d', {} },
    { 'n', '<leader>D', '"+D', {} },
    -- Disable highlight
    { 'n', '<esc>', ':noh<cr>', opts },
    -- Use '0' key to switch between beginning of line and first column
    { 'n', '0', 'virtcol(".")== 1 ? "^" : "0"', { expr = true } },
    -- Tabs
    { 'n', '<a-l>', ':tabnext<cr>', opts },
    { 'n', 'tl', ':tabnext<cr>', opts },
    { 'n', '<a-h>', ':tabprev<cr>', opts },
    { 'n', 'th', ':tabprev<cr>', opts },
    { 'n', '<a-L>', ':tabm +1<cr>', opts },
    { 'n', '<a-H>', ':tabm -1<cr>', opts },
    -- Move to next window
    { 'n', '<c-l>', ':wincmd w<cr>', opts },
    -- Move to previous window
    { 'n', '<c-h>', ':wincmd W<cr>', opts },
    -- Terminal enter normal mode
    { 't', '<esc>', '<c-\\><c-n>', opts },
    -- Paste latest yanked text
    { { 'n', 'x' }, '.p', '"0p', opts },

    -- Scroll
    { { 'n', 'v' }, 'm', '10<c-e>', opts },
    { { 'n', 'v' }, ',', '10<c-y>', opts },
    { { 'n', 'v' }, 'H', '^', opts },
    { { 'n', 'v' }, 'L', '$', opts },
    -- Replace for ← → in insert mode
    { { 'i' }, '<c-l>', '<right>', opts },
    { { 'i' }, '<c-h>', '<left>', opts },
    { { 'i' }, '<a-h>', '<C-\\><C-O>b', opts },
    { { 'i' }, '<a-l>', '<C-\\><C-O>w', opts },

    { { 'n', 'v' }, '<a-c>', ':Make!<cr>', opts },
}

M.ctrlsf = {
    { 'n', '<C-F>f', '<Plug>CtrlSFPrompt', {} },
    { 'v', '<C-F>f', '<Plug>CtrlSFVwordPath', {} },
    { 'v', '<C-F>F', '<Plug>CtrlSFVwordExec', {} },
    { 'n', '<C-F>n', '<Plug>CtrlSFCwordPath', {} },
    { 'n', '<C-F>p', '<Plug>CtrlSFPwordPath', {} },
    { 'n', '<C-F>o', ':CtrlSFOpen<cr>', { noremap = true } },
    { 'n', '<C-F>t', ':CtrlSFToggle<cr>', { noremap = true } },
    { 'i', '<C-F>t', '<Esc>:CtrlSFToggle<cr>', { noremap = true } },
}

M.easyalign = {
    { 'n', 'ga', "<Plug>(EasyAlign)", {} },
    { 'v', 'ga', "<Plug>(EasyAlign)", {} },
    { 'x', 'ga', "<Plug>(EasyAlign)", {} },
}

M.lspconfig = {
    { 'n', '<Leader>rn', vim.lsp.buf.rename, opts },
    { 'n', 'gr', vim.lsp.buf.references, opts },
    { 'n', 'gD', vim.lsp.buf.declaration, opts },
    { 'n', 'gd', vim.lsp.buf.definition, opts },
    { 'n', 'gt', vim.lsp.buf.type_definition, opts },
    { 'n', 'gi', vim.lsp.buf.implementation, opts },
    { 'n', 'gf', function() vim.lsp.buf.format { async = true } end, opts },
    { 'n', '<Leader>df', vim.diagnostic.goto_next, opts },
    { 'n', 'K', vim.lsp.buf.hover, opts },
    { 'i', '<C-k>', vim.lsp.buf.signature_help, opts },
    { 'n', '<Leader>a', vim.lsp.buf.code_action, opts },
}

local has_dap, dap = pcall(require, 'dap')
if has_dap then
    M.dap = {
        { 'n', '<F5>', dap.continue, {} },
        { 'n', '<F9>', dap.step_over, {} },
        { 'n', '<F10>', dap.step_into, {} },
        { 'n', '<F11>', dap.step_out, {} },
        { 'n', '<Leader>b', dap.toggle_breakpoint, {} },
        { 'n', '<Leader>B',
            function()
                dap.set_breakpoint(vim.fn.input('Condition: '))
            end,
            opts
        },
    }
end

local has_dapui, dapui = pcall(require, 'dapui')
if has_dapui then
    M.dapui = {
        { 'n', '<F12>', dapui.toggle, opts },
    }
end

local nvterm_toggle = function() require("nvterm.terminal").toggle "float" end
M.nvterm = {
    { { 'n', 't', 'i' }, '<A-i>', nvterm_toggle, opts },
    { { 'n', 't' }, '<C-t>', nvterm_toggle, opts },
}

local exists_telescope, telescope = pcall(require, 'telescope.builtin')
if exists_telescope then
    local exts = require('telescope').extensions
    local themes = require('telescope.themes')
    local ivy = themes.get_ivy()
    M.telescope = {
        { 'n', '<leader>ff', function() telescope.find_files(ivy) end, opts },
        { 'n', '<leader>fg', function() telescope.live_grep(ivy) end, opts },
        { 'n', '<leader>fu', function() telescope.buffers(ivy) end, opts },
        { 'n', '<leader>fh', function() telescope.help_tags(ivy) end, opts },
        { 'n', '<leader>fm',
            function() telescope.man_pages({ sections = { "1", "2", "3" } }) end,
            opts },
        { 'n', '<leader>fb', function() exts.file_browser.file_browser(ivy) end, opts },
        { 'n', '<C-c>', function() exts.file_browser.file_browser(ivy) end, opts },
        { 'n', '<leader>fd', '<cmd>Telescope diagnostics<cr>', opts },
        { 'n',
            '<C-space>',
            function()
                telescope.current_buffer_fuzzy_find(ivy)
            end,
            opts },
    }
end

return M
