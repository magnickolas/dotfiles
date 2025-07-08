return {
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
}
