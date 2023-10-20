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
  vim.o.cursorline = false
  vim.cmd("colorscheme " .. colorscheme)
  vim.cmd([[highlight StatusLine ctermbg=NONE cterm=NONE guibg=NONE gui=NONE]])
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
  -- vim.o.foldmethod = "marker"
  vim.o.foldlevel = 99
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
  -- color line and column in red
  vim.o.rulerformat = "%50(%=%#LineNr#%.50F "
    .. rulericons[math.random(#rulericons)]
    .. " %{strlen(&ft)?&ft:'none'} %l:%c %)"
  -- vim.o.rulerformat = "%50(%= " .. rulericons[math.random(#rulericons)] .. " %{strlen(&ft)?&ft:'none'} %l:%c %)"
  -- vim.o.rulerformat = "%20(%= %{strlen(&ft)?&ft:'none'} "
  --   .. rulericons[math.random(#rulericons)]
  --   .. " %m%r%6(%l:%c%) %)"

  if vim.fn.executable("rg") then
    vim.opt.grepprg = "rg --vimgrep --no-heading"
    vim.opt.grepformat = "%f:%l:%c:%m,%f:%l:%m"
  end

  vim.cmd([[ca tn tabnew]])
end

M.autocommands = function()
  local function nvim_create_augroups(definitions)
    for group_name, definition in pairs(definitions) do
      vim.api.nvim_create_augroup(group_name, {})
      for _, def in ipairs(definition) do
        local command_name = table.remove(def, 1)
        local pattern = table.remove(def, 1)
        local opts = require("utils").update({ pattern = pattern }, def)
        vim.api.nvim_create_autocmd(command_name, opts)
      end
    end
  end
  local autocmds = {
    exit = {
      -- https://github.com/neovim/neovim/issues/6005
      -- In some terminals (e.g. Alacritty, Konsole),
      -- the cursor is getting set to `block` after neovim exit.
      --
      -- Hack it for now and force set the desirable cursor style
      -- right before Vim exit
      {
        "VimLeave",
        "*",
        callback = function()
          vim.o.guicursor = "a:ver1-blinkon0"
        end,
      },
    },
    spell = {
      {
        "FileType",
        "gitcommit,markdown",
        callback = function()
          vim.wo.spell = true
        end,
      },
    },
    cpp = {
      -- disable buggy treesitter indentation
      {
        "FileType",
        "cpp",
        callback = function()
          vim.bo.indentexpr = ""
        end,
      },
    },
    ftdetect = {
      {
        { "BufEnter", "BufRead" },
        "*.ii",
        callback = function()
          vim.bo.filetype = "cpp"
        end,
      },
      {
        { "BufEnter", "BufRead" },
        "*.go",
        callback = function()
          require("plugins.lsp.format").autoformat = false
        end,
      },
    },
    help = {
      {
        "FileType",
        "help",
        callback = function()
          vim.api.nvim_buf_set_keymap(0, "n", "<Return>", "<C-]>", { noremap = true })
        end,
      },
      {
        "FileType",
        "help,man",
        callback = function()
          vim.api.nvim_buf_set_keymap(0, "n", "q", "<cmd>q<cr>", { noremap = true })
        end,
      },
    },
  }
  nvim_create_augroups(autocmds)
end

return M
