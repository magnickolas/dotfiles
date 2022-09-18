local dap = require('dap')
dap.adapters.python = {
  type = 'executable';
  command = 'python3';
  args = { '-m', 'debugpy.adapter' };
}
dap.configurations.python = {
  {
    type = 'python';
    request = 'launch';
    name = "Launch file";
    program = "${file}";
    pythonPath = function()
      return 'python3'
    end;
  },
}
dap.adapters.cppdbg = {
  id = 'cppdbg',
  type = 'executable',
  command = vim.fn.expand('~') .. '/.config/nvim/dap/cpp/OpenDebugAD7',
}
dap.configurations.cpp = {
  {
    name = "Launch file",
    type = "cppdbg",
    request = "launch",
    program = '${fileDirname}/${fileBasenameNoExtension}',
    -- program = function()
    --   return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    -- end,
    cwd = '${workspaceFolder}',
    stopAtEntry = true,
    externalTerminal = true,
    setupCommands = {
      {
         text = '-enable-pretty-printing',
         description =  'enable pretty printing',
         ignoreFailures = false
      },
    },
  },
  -- {
  --   name = 'Attach to gdbserver :1234',
  --   type = 'cppdbg',
  --   request = 'launch',
  --   MIMode = 'gdb',
  --   miDebuggerServerAddress = 'localhost:1234',
  --   miDebuggerPath = '/usr/bin/gdb',
  --   cwd = '${workspaceFolder}',
  --   program = '${fileDirname}/${fileBasenameNoExtension}',
  --   -- program = function()
  --   --   return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
  --   -- end,
  --   setupCommands = {
  --     {
  --        text = '-enable-pretty-printing',
  --        description =  'enable pretty printing',
  --        ignoreFailures = false
  --     },
  --   },
  -- },
}
dap.configurations.c = dap.configurations.cpp

dap.adapters.rustdbg = {
  id = 'rustdbg',
  type = 'executable',
  command = vim.fn.expand('~') .. '/.config/nvim/dap/rust/codelldb',
}
dap.configurations.rust = {
  {
    name = "Launch file",
    type = "rustdbg",
    request = "launch",
    -- program = '${fileDirname}/${fileBasenameNoExtension}',
    program = function()
      return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    end,
    cwd = '${workspaceFolder}',
    stopAtEntry = true,
    externalTerminal = true,
    setupCommands = {
      {
         text = '-enable-pretty-printing',
         description =  'enable pretty printing',
         ignoreFailures = false
      },
    },
  },
}
