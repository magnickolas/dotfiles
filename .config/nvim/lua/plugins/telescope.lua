local function exts()
  return require("telescope").extensions
end
local function theme()
  return require("telescope.themes").get_ivy()
end
local function tb()
  return require("telescope.builtin")
end
local function project_files(opts)
  if vim.loop.fs_stat(".git") then
    opts.show_untracked = true
    tb().git_files(opts)
  else
    local client = vim.lsp.get_active_clients()[1]
    if client then
      opts.cwd = client.config.root_dir
    end
    tb().find_files(opts)
  end
end

local function show_hidden(opts)
  return require("utils").update(opts, { hidden = true })
end

return {
  "nvim-telescope/telescope.nvim",
  cmd = "Telescope",
  dependencies = {
    { "nvim-telescope/telescope-file-browser.nvim" },
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
  },
  keys = {
    {
      "<leader>ff",
      function()
        project_files(show_hidden(theme()))
      end,
      desc = "Find File in project dir",
    },
    {
      "<leader>fc",
      function()
        tb().find_files(show_hidden(theme()))
      end,
      desc = "Find File in cwd",
    },
    {
      "<leader>fg",
      function()
        tb().live_grep(show_hidden(theme()))
      end,
      desc = "",
    },
    {
      "<leader>ws",
      function()
        tb().lsp_workspace_symbols(theme())
      end,
      desc = "",
    },
    {
      "<leader>ds",
      function()
        tb().lsp_document_symbols(theme())
      end,
      desc = "",
    },
    {
      "<leader>?",
      function()
        tb().oldfiles(theme())
      end,
      desc = "",
    },
    {
      "<leader><space>",
      function()
        tb().buffers(theme())
      end,
      desc = "",
    },
    {
      "<leader>fh",
      function()
        tb().help_tags(theme())
      end,
      desc = "",
    },
    {
      "<leader>fm",
      function()
        tb().man_pages({ sections = { "1", "2", "3", "3p" } })
      end,
      desc = "",
    },
    {
      "<leader>fb",
      function()
        exts().file_browser.file_browser(show_hidden(theme()))
      end,
      desc = "",
    },
    {
      "<leader>fd",
      function()
        tb().diagnostics(theme())
      end,
      desc = "",
    },
    {
      "<leader>/",
      function()
        tb().current_buffer_fuzzy_find(theme())
      end,
      desc = "",
    },
    { "<leader>gc", "<cmd>Telescope git_commits<CR>", desc = "commits" },
    { "<leader>gs", "<cmd>Telescope git_status<CR>", desc = "status" },
  },
  opts = {
    defaults = {
      layout_strategy = "horizontal",
      layout_config = { prompt_position = "top" },
      sorting_strategy = "ascending",
      winblend = 0,
    },
  },
  config = function(_, opts)
    local telescope = require("telescope")
    telescope.setup(opts)
    telescope.load_extension("fzf")
  end,
}
