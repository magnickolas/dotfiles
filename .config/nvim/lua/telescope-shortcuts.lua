map('n', '<Leader>ff', "<cmd>lua require('telescope.builtin').find_files()<cr>", opts)
map('n', '<Leader>fg', "<cmd>lua require('telescope.builtin').live_grep()<cr>", opts)
map('n', '<Leader>fb', "<cmd>lua require('telescope.builtin').buffers()<cr>", opts)
map('n', '<Leader>fh', "<cmd>lua require('telescope.builtin').help_tags()<cr>", opts)
