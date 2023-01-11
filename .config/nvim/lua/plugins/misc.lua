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
      create_mappings = true,
      operator_mapping = "<leader>/",
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
      key_labels = { ["<leader>"] = "SPC" },
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)
      wk.register({
        mode = { "n", "v" },
        ["g"] = { name = "+goto" },
        ["]"] = { name = "+next" },
        ["["] = { name = "+prev" },
        ["<leader>b"] = { name = "+buffer" },
        ["<leader>c"] = { name = "+code" },
        ["<leader>f"] = { name = "+file" },
        ["<leader>g"] = { name = "+git" },
        ["<leader>h"] = { name = "+help" },
        ["<leader>n"] = { name = "+noice" },
        ["<leader>o"] = { name = "+open" },
        ["<leader>q"] = { name = "+quit/session" },
        ["<leader>s"] = { name = "+search" },
        ["<leader>t"] = { name = "+toggle" },
        ["<leader>x"] = { name = "+diagnostics/quickfix" },
        ["<leader>w"] = { name = "+windows" },
        ["<leader><tab>"] = { name = "+tabs" },
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
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
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
}
