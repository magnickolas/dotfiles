if vim.fn.isdirectory(vim.fn.expand("~/.vim/undo")) == 0 then
  vim.fn.mkdir(vim.fn.expand("~/.vim/undo"), "", 0700)
end
vim.opt.undodir = vim.fn.expand("~/.vim/undo")
vim.opt.undofile = true

if vim.fn.isdirectory(vim.fn.expand("~/.vim/backup")) == 0 then
  vim.fn.mkdir(vim.fn.expand("~/.vim/backup"), "")
end
vim.opt.backupdir = vim.fn.expand("~/.vim/backup")
vim.opt.writebackup = true
vim.opt.backup      = false
