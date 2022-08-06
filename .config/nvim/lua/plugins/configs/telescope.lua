local actions = require 'telescope.actions'
local telescope = require 'telescope'

telescope.setup {
  defaults = {
    history = {
      path = '~/.local/share/nvim/databases/telescope_history.sqlite3',
      limit = 100,
    },
    mappings = {
      i = {
        ['<C-j>'] = actions.cycle_history_next,
        ['<C-k>'] = actions.cycle_history_prev,
      }
    },
  },
}

telescope.load_extension('smart_history')
