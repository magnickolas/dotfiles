local M = {}

M.load_mappings = function(m)
  for _, mappings in pairs(m) do
    for _, mapping in pairs(mappings) do
      vim.keymap.set(mapping[1], mapping[2], mapping[3], mapping[4])
    end
  end
end

M.init_screen = function()
  vim.opt.shortmess:append({ I = true })
end

M.set_colorscheme = function(colorscheme)
  vim.o.background = "dark"
  vim.cmd([[highlight StatusLine ctermbg=NONE cterm=NONE guibg=NONE gui=NONE]])
  vim.o.cursorline = false
  vim.cmd("colorscheme " .. colorscheme)
end

M.configure_backup = function()
  local undo_dir = vim.fn.expand("~/.vim/undo")
  if vim.fn.isdirectory(undo_dir) == 0 then
    vim.fn.mkdir(undo_dir, "p")
  end
  vim.opt.undodir = undo_dir
  vim.opt.undofile = true
  vim.opt.backup = true
  vim.opt.backupdir = vim.fn.stdpath("state") .. "/backup"
end

M.configure_editor = function()
  vim.o.shell = "zsh"
  vim.o.title = true
  vim.o.startofline = false
  vim.o.tabstop = 4
  vim.o.shiftwidth = 4
  vim.o.expandtab = true
  vim.o.autoindent = false
  vim.o.smartindent = false
  vim.o.ignorecase = false
  vim.o.number = false
  vim.o.hidden = true
  vim.o.completeopt = "menuone,noinsert,noselect"
  vim.o.showmode = false
  vim.o.showcmd = false
  vim.o.foldmethod = "marker"
  vim.o.foldlevel = 0
  vim.o.autochdir = false
  vim.o.splitright = true
  vim.o.splitbelow = true
  vim.o.updatetime = 750
  vim.o.mouse = "a"

  vim.o.laststatus = 0
  vim.o.cmdheight = 1
  vim.o.ruler = true
  local rulericons = { "☃ ", "🎅", "🎄", "❄ " }
  math.randomseed(os.time())
  vim.o.rulerformat = "%50(%=%#LineNr#%.50F "
    .. rulericons[math.random(#rulericons)]
    .. " %{strlen(&ft)?&ft:'none'} %l:%c %)"

  if vim.fn.executable("rg") then
    vim.opt.grepprg = "rg --vimgrep --no-heading"
    vim.opt.grepformat = "%f:%l:%c:%m,%f:%l:%m"
  end

  vim.cmd([[ca tn tabnew]])
end

return M
