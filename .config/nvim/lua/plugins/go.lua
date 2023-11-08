return {
  "ray-x/go.nvim",
  dependencies = {
    "nvim-treesitter/nvim-treesitter",
  },
  ft = "go",
  config = function()
    require("go").setup()
    local format_sync_grp = vim.api.nvim_create_augroup("GoImport", {})
    vim.api.nvim_create_autocmd("BufWritePre", {
      pattern = "*.go",
      callback = function()
        require("go.format").goimport()
      end,
      group = format_sync_grp,
    })
  end,
}
