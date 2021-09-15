local undo_dir = vim.fn.expand("~/.vim/undo")

if vim.fn.isdirectory(undo_dir) == 0 then
  vim.fn.mkdir(undo_dir, "")
end
vim.opt.undodir = undo_dir
vim.opt.undofile = true

vim.opt.writebackup = false
vim.opt.backup      = false
