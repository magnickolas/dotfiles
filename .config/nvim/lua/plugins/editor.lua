return {
  "nvim-lua/plenary.nvim",
  "dstein64/vim-startuptime",
  {
    "andymass/vim-matchup",
    event = "BufReadPost",
    config = function()
      vim.g.matchup_matchparen_offscreen = { method = "status_manual" }
    end,
  },
  {
    "terrortylor/nvim-comment",
    event = "VeryLazy",
    opts = {
      marker_padding = true,
      comment_empty = true,
      create_mappings = false,
      hook = nil,
    },
    config = function(_, opts)
      require("nvim_comment").setup(opts)
    end,
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      plugins = { spelling = true },
      replace = { ["<leader>"] = "SPC" },
      preset = "modern",
      delay = 0,
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)
      wk.add({
        mode = { "n", "v" },
        { "g", desc = "+goto" },
        { "]", desc = "+next" },
        { "[", desc = "+prev" },
        { "<leader>c", desc = "+code" },
        { "<leader>f", desc = "+file" },
        { "<leader>g", desc = "+git" },
        { "<leader>t", desc = "+toggle" },
        { "<leader>x", desc = "+diagnostics/quickfix" },
        { "<leader>w", desc = "+windows" },
      })
    end,
  },
  {
    "ggandor/leap.nvim",
    event = "VeryLazy",
    keys = {
      {
        "f",
        function()
          require("leap").leap({ target_windows = { vim.fn.win_getid() } })
        end,
        silent = true,
        mode = "n",
      },
    },
    opts = {
      max_phase_one_targets = nil,
      highlight_unlabeled_phase_one_targets = true,
      max_highlighted_traversal_targets = 10,
      case_sensitive = false,
      equivalence_classes = { " \t\r\n" },
      substitute_chars = {},
      special_keys = {
        repeat_search = "<enter>",
        next_phase_one_target = "<enter>",
        next_target = { "<enter>", ";" },
        prev_target = { "<tab>", "," },
        next_group = "<space>",
        prev_group = "<tab>",
        multi_accept = "<enter>",
        multi_revert = "<backspace>",
      },
    },
    config = function(_, opts)
      vim.api.nvim_set_hl(0, "LeapBackdrop", { link = "Comment" })
      vim.api.nvim_set_hl(0, "LeapMatch", {
        fg = "white",
        bold = true,
        nocombine = true,
      })
      local leap = require("leap")
      for k, v in pairs(opts) do
        leap.opts[k] = v
      end
    end,
  },
  {
    "alvarosevilla95/luatab.nvim",
    dependencies = { "kyazdani42/nvim-web-devicons" },
    event = "VeryLazy",
    config = function()
      local luatab = require("luatab")
      local devicon_def = luatab.helpers.devicon
      local devicon = function(bufnr, isSelected)
        return devicon_def(bufnr, false)
      end
      luatab.setup({ devicon = devicon })
    end,
  },
  {
    "smjonas/inc-rename.nvim",
    cmd = "IncRename",
    config = true,
  },
  {
    "iamcco/markdown-preview.nvim",
    build = "cd app && yarn install",
  },
  {
    "junegunn/vim-easy-align",
    event = "VeryLazy",
    keys = {
      { "ga", "<Plug>(EasyAlign)", mode = { "n", "x" }, desc = "Align" },
    },
  },
  {
    "simnalamburt/vim-mundo",
  },
  {
    "norcalli/nvim-terminal.lua",
    event = "VeryLazy",
    config = true,
  },
  "mg979/vim-visual-multi",
  {
    "klen/nvim-config-local",
    opts = {
      -- Default configuration (optional)
      config_files = { ".nvimrc.lua", ".nvimrc" }, -- Config file patterns to load (lua supported)
      hashfile = vim.fn.stdpath("data") .. "/config-local", -- Where the plugin keeps files data
      autocommands_create = true, -- Create autocommands (VimEnter, DirectoryChanged)
      commands_create = true, -- Create commands (ConfigSource, ConfigEdit, ConfigTrust, ConfigIgnore)
      silent = true, -- Disable plugin messages (Config loaded/ignored)
      lookup_parents = true, -- Lookup config files in parent directories
    },
  },
  "NoahTheDuke/vim-just",
  "tikhomirov/vim-glsl",
  "tpope/vim-fugitive",
  {
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
  },
  {
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
  {
    "Saecki/crates.nvim",
    tag = "v0.3.0",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = true,
  },
  {
    "folke/todo-comments.nvim",
    event = "VeryLazy",
    keys = {
      {
        "<leader>ft",
        ":TodoTelescope<CR>",
        desc = "TODOs",
      },
    },
    dependencies = "nvim-lua/plenary.nvim",
    opts = {
      signs = false,
      search = {
        pattern = [[\b(KEYWORDS)\b]],
      },
    },
  },
  {
    "p00f/godbolt.nvim",
    opts = {
      languages = {
        cpp = { compiler = "g122", options = {} },
        c = { compiler = "cg122", options = {} },
        rust = { compiler = "r1650", options = {} },
        -- any_additional_filetype = { compiler = ..., options = ... },
      },
      quickfix = {
        enable = false, -- whether to populate the quickfix list in case of errors
        auto_open = false, -- whether to open the quickfix list in case of errors
      },
      url = "https://godbolt.org", -- can be changed to a different godbolt instance
    },
  },
  "pest-parser/pest.vim",
  "hashivim/vim-terraform",
  "folke/trouble.nvim",
}
