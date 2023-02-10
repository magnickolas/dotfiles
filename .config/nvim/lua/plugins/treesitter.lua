local M = {
  "nvim-treesitter/nvim-treesitter",
  config = function()
    require("nvim-treesitter.configs").setup({
      query_linter = {
        enable = true,
        use_virtual_text = true,
        lint_events = { "BufWrite", "CursorHold" },
      },
    })
  end,
  dependencies = {
    "nvim-treesitter/playground",
    config = function()
      local ts = require("nvim-treesitter.configs")
      ts.setup({
        highlight = {
          enable = true,
        },
        indent = { enable = true },
        playground = {
          enable = true,
          disable = {},
          updatetime = 25,
          persist_queries = false,
          keybindings = {
            -- probably not relevant
          },
        },
      })
    end,
  },
}

-- local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
-- parser_config.zimbu = {
--   install_info = {
--     url = "~/.config/nvim/treesitter/tree-sitter-markdown/", -- local path or git repo
--     -- files = {"src/parser.c"},
--     -- optional entries:
--     branch = "split_parser", -- default branch in case of git repo if different from master
--     generate_requires_npm = true, -- if stand-alone parser without npm dependencies
--     requires_generate_from_grammar = false, -- if folder contains pre-generated src/parser.c
--   },
--   filetype = "markdown", -- if filetype does not match the parser name
-- }

return M
