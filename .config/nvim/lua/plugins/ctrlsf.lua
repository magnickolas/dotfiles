return {
  {
    "dyng/ctrlsf.vim",
    cmd = "CtrlSF",
    keys = {
      { "<C-F>f", "<Plug>CtrlSFPrompt", mode = "n" },
      { "<C-F>f", "<Plug>CtrlSFVwordPath", mode = "v" },
      { "<C-F>F", "<Plug>CtrlSFVwordExec", mode = "v" },
      { "<C-F>n", "<Plug>CtrlSFCwordPath", mode = "n" },
      { "<C-F>p", "<Plug>CtrlSFPwordPath", mode = "n" },
      { "<C-F>o", ":CtrlSFOpen<CR>", mode = "n" },
      { "<C-F>t", ":CtrlSFToggle<CR>", mode = "n" },
      { "<C-F>t", "<Esc>:CtrlSFToggle<CR>", mode = "i" },
    },
  },
}
