-- Move to previous/next
map('n', '<A-,>', ':BufferLineCyclePrev<CR>', opts)
map('n', '<A-.>', ':BufferLineCycleNext<CR>', opts)
map('n', 'th', ':BufferLineCyclePrev<CR>', opts)
map('n', 'tl', ':BufferLineCycleNext<CR>', opts)
-- Re-order to previous/next
map('n', '<A-<>', ':BufferLineMovePrev<CR>', opts)
map('n', '<A->>', ' :BufferLineMoveNext<CR>', opts)
-- Goto buffer in position...
map('n', '<A-1>', ':BufferLineGoToBuffer 1<CR>', opts)
map('n', '<A-2>', ':BufferLineGoToBuffer 2<CR>', opts)
map('n', '<A-3>', ':BufferLineGoToBuffer 3<CR>', opts)
map('n', '<A-4>', ':BufferLineGoToBuffer 4<CR>', opts)
map('n', '<A-5>', ':BufferLineGoToBuffer 5<CR>', opts)
map('n', '<A-6>', ':BufferLineGoToBuffer 6<CR>', opts)
map('n', '<A-7>', ':BufferLineGoToBuffer 7<CR>', opts)
map('n', '<A-8>', ':BufferLineGoToBuffer 8<CR>', opts)
map('n', '<A-9>', ':BufferLineGoToBuffer 9<CR>', opts)
-- Close buffer
map('n', '<A-c>', ':BufferLinePickClose<CR>', opts)
-- Wipeout buffer
--                 :BufferWipeout<CR>
-- Close commands
--                 :BufferCloseAllButCurrent<CR>
--                 :BufferCloseBuffersLeft<CR>
--                 :BufferCloseBuffersRight<CR>
-- Magic buffer-picking mode
map('n', '<C-p>', ':BufferLinePick<CR>', opts)
-- Sort automatically by...
map('n', '<Space>bd', ':BufferLineSortByRelativeDirectory<CR>', opts)

-- Copy to clipboard
map('n', '<leader>y', '"+y', opts)
map('n', '<leader>Y', '"+yg_', opts)

-- Paste from clipboard
map('n', '<leader>p', '"+p', opts)
map('n', '<leader>P', '"+P', opts)

-- Delete to clipboard
map('n', '<leader>d', '"+d', opts)
map('n', '<leader>D', '"+D', opts)
