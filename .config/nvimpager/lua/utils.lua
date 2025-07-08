return {
  update = function(x, y)
    return vim.tbl_extend("force", x, y)
  end,
}
